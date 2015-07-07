{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Reflex.QML.Internal where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Dependent.Sum
import Data.IORef
import Data.Maybe
import Graphics.QML
import Prelude
import Reflex.Class hiding (constant)
import Reflex.Dynamic
import Reflex.Host.Class
import Reflex.Spider

import qualified Data.Foldable as F
import qualified Data.Traversable as T

data ObjectSpec m t = ObjectSpec
  { members :: [Member ()]
  , outputAction :: ObjRef () -> [Event t (m ())]
  }
instance Applicative m => Monoid (ObjectSpec m t) where
  mempty = ObjectSpec mempty mempty
  mappend (ObjectSpec a b) (ObjectSpec a' b') = ObjectSpec (a <> a') (b <> b')

data Env t = Env
  { inputAction :: Chan (DSum (EventTrigger t))
  }

newtype QApp t m a = QApp
  { unApp :: ReaderT (Env t) (WriterT (ObjectSpec m t) m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

instance MonadTrans (QApp t) where
  lift = QApp . lift . lift

instance MonadSample t m => MonadSample t (QApp t m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (QApp t m) where
  hold v = lift . hold v

newEventWithFire :: (MonadReflexCreateTrigger t m, MonadIO m)
                 => QApp t m (Event t a, a -> IO Bool)
newEventWithFire = do
  triggerRef <- liftIO $ newIORef Nothing
  event <- lift . newEventWithTrigger $ \t ->
    writeIORef triggerRef Nothing <$ writeIORef triggerRef (Just t)
  Env chan <- QApp ask
  let fire v = do
        trigger <- readIORef triggerRef
        case trigger of
          Nothing -> pure False
          Just x -> True <$ writeChan chan (x :=> v)
  pure (event, fire)

performEvent :: (Reflex t, MonadIO m, MonadReflexCreateTrigger t m)
             => Event t (m a) -> QApp t m (Event t a)
performEvent event = do
  triggerRef <- liftIO $ newIORef Nothing
  result <- lift $ newEventWithTrigger $ \t -> do
    writeIORef triggerRef (Just t)
    pure $ writeIORef triggerRef Nothing
  Env chan <- QApp ask
  let fireResult v = liftIO $ do
        maybeTrigger <- readIORef triggerRef
        F.for_ maybeTrigger $ \t -> writeChan chan (t :=> v)
  QApp $ tell . ObjectSpec [] $ const [ffor event (>>= fireResult)]
  pure result

performDynamic :: (Reflex t, MonadIO m, MonadReflexCreateTrigger t m, MonadSample t m,
                  MonadHold t m)
               => Dynamic t (m a) -> QApp t m (Dynamic t a)
performDynamic dyn = do
  i <- lift . join . sample $ current dyn
  e <- performEvent $ updated dyn
  lift $ holdDyn i e

subApp :: (MonadIO m, Reflex t, MonadReflexCreateTrigger t m, MonadHold t m
            ,MonadSample t m)
          => Dynamic t (QApp t m a) -> QApp t m (Dynamic t AnyObjRef, Dynamic t a)
subApp appD = do
  env <- QApp ask
  let make (QApp app) = do
        (a, ObjectSpec ms outputs) <- runWriterT (runReaderT app env)
        obj <- liftIO $ newClass ms >>= flip newObject ()
        pure (anyObjRef obj, (mergeWith (>>) $ outputs obj, a))
  dyn <- performDynamic =<< mapDyn make appD
  (objDyn, (outputsDyn, aDyn)) <- T.traverse splitDyn =<< splitDyn dyn
  QApp $ tell . ObjectSpec [] $ const [switchPromptlyDyn outputsDyn]
  pure (objDyn, aDyn)

runApp :: EngineConfig -> QApp Spider (HostFrame Spider) () -> RunQML ()
runApp config (QApp app) = do
  -- This variable will hold the context object as soon as the reflex thread
  -- has initialized itself
  objVar <- liftIO newEmptyMVar

  -- The reflex thread. Since running the QML engine blocks the main thread
  -- (we can't run use runEngineAsync because then we don't know when to stop),
  -- a thread is used to process the reflex events.
  let reflexThread = runSpiderHost $ do
        chan <- liftIO newChan
        ObjectSpec ms outputs <- runHostFrame $ execWriterT (runReaderT app (Env chan))
        obj <- liftIO $ newClass ms >>= flip newObject ()
        liftIO $ putMVar objVar obj
        allOutputs <- subscribeEvent $ mergeWith (>>) $ outputs obj
        forever $ do
          nextInputEvent <- liftIO $ readChan chan
          join $ fireEventsAndRead [nextInputEvent] $ maybe (pure ()) runHostFrame <$>
            (readEvent allOutputs >>= T.sequenceA)

  -- Now start the reflex thread and run the QML engine in the main thread after the
  -- reflex thread has initialized itself far enough to produce the context object.
  liftIO . withAsync reflexThread $ \_ -> do
    obj <- takeMVar objVar
    requireEventLoop $ runEngine config
      { contextObject = Just $ anyObjRef obj
      }
