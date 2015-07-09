{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.QML.Internal.AppHost where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Dependent.Sum
import Data.IORef
import Data.Maybe
import Data.Semigroup.Applicative
import Prelude
import Reflex.Class hiding (constant)
import Reflex.Dynamic
import Reflex.Host.Class
import Reflex.Spider

import qualified Data.DList as DL
import qualified Data.Foldable as F
import qualified Data.Traversable as T

--------------------------------------------------------------------------------
data AppEnv t = AppEnv
  { envEventChan :: Chan [DSum (EventTrigger t)]
  }

type AppPerformAction t = HostFrame t (DL.DList (DSum (EventTrigger t)))
data AppInfo t = AppInfo
  { eventsToPerform :: DL.DList (Event t (AppPerformAction t))
  , eventsToQuit :: DL.DList (Event t ())
  }
instance Monoid (AppInfo t) where
  mempty = AppInfo mempty mempty
  mappend (AppInfo a b) (AppInfo a' b') = AppInfo (mappend a a') (mappend b b')

newtype AppHost t a = AppHost
  { unAppHost :: ReaderT (AppEnv t) (WriterT (Ap (HostFrame t) (AppInfo t)) (HostFrame t)) a
  }
deriving instance ReflexHost t => Functor (AppHost t)
deriving instance ReflexHost t => Applicative (AppHost t)
deriving instance ReflexHost t => Monad (AppHost t)
deriving instance ReflexHost t => MonadHold t (AppHost t)
deriving instance ReflexHost t => MonadSample t (AppHost t)
deriving instance ReflexHost t => MonadReflexCreateTrigger t (AppHost t)
deriving instance (MonadIO (HostFrame t), ReflexHost t) => MonadIO (AppHost t)
deriving instance ReflexHost t => MonadFix (AppHost t)

execAppHostFrame :: ReflexHost t => AppEnv t -> AppHost t a -> HostFrame t (AppInfo t)
execAppHostFrame env = fmap fst . runAppHostFrame env

runAppHostFrame :: ReflexHost t => AppEnv t -> AppHost t a -> HostFrame t (AppInfo t, a)
runAppHostFrame env app = do
  (a, minfo) <- runWriterT . flip runReaderT env . unAppHost $ app
  (, a) <$> getApp minfo

hostApp :: (ReflexHost t, MonadIO m, MonadReflexHost t m) => AppHost t () -> m ()
hostApp app = do
  env <- AppEnv <$> liftIO newChan
  AppInfo{..} <- runHostFrame $ execAppHostFrame env app
  nextActionEvent <- subscribeEvent $ mergeWith (liftA2 (<>)) $ DL.toList eventsToPerform
  quitEvent <- subscribeEvent $ mergeWith mappend $ DL.toList eventsToQuit

  let
    go [] = return ()
    go triggers = do
      (nextAction, continue) <- lift $ fireEventsAndRead triggers $
        (,) <$> eventValue nextActionEvent <*> fmap isNothing (readEvent quitEvent)
      guard continue
      maybe (return mempty) (lift . runHostFrame) nextAction >>= go . DL.toList

    eventValue :: MonadReadEvent t m => EventHandle t a -> m (Maybe a)
    eventValue = readEvent >=> T.sequenceA

  void . runMaybeT . forever $ do
    nextInput <- liftIO . readChan $ envEventChan env
    go nextInput
  return ()

--------------------------------------------------------------------------------
class (ReflexHost t, MonadSample t m, MonadHold t m, MonadReflexCreateTrigger t m,
       MonadIO m, MonadIO (HostFrame t), MonadFix m, MonadFix (HostFrame t))
      => MonadAppHost t m | m -> t where
  getAsyncFire :: m ([DSum (EventTrigger t)] -> IO ())
  performPostBuild_
    :: HostFrame t (DL.DList (Event t (AppPerformAction t)), DL.DList (Event t ())) -> m ()
  performAppHost :: Dynamic t (m a) -> m (Event t a)

instance (ReflexHost t, MonadIO (HostFrame t)) => MonadAppHost t (AppHost t) where
  getAsyncFire = AppHost $ fmap liftIO . writeChan . envEventChan <$> ask

  performPostBuild_ mevent = AppHost . tell . Ap $ uncurry AppInfo <$> mevent

  performAppHost appDyn = do
    env <- AppHost ask
    (updatedEvents, revent) <- fmap splitE . performEvent $
      fmap (overFst getEvents) . runAppHostFrame env <$> updated appDyn
    performPostBuild_ $ do
      initialEvents <- fmap getEvents . execAppHostFrame env =<< sample (current appDyn)
      let (initialToPerform, initialToQuit) = initialEvents
          (updatedToPerform, updatedToQuit) = splitE updatedEvents
      toPerform <- switch <$> hold initialToPerform updatedToPerform
      toQuit    <- switch <$> hold initialToQuit updatedToQuit
      pure (pure toPerform, pure toQuit)
    return revent
   where
    overFst f (a,b) = (f a, b)

    getEvents :: AppInfo t -> (Event t (AppPerformAction t), Event t ())
    getEvents AppInfo{..} =
      ( mergeWith (liftA2 (<>)) $ DL.toList eventsToPerform
      , leftmost $ DL.toList eventsToQuit
      )

newEventWithFire :: (MonadIO n, MonadAppHost t m)
                => (DL.DList (DSum (EventTrigger t)) -> n b)
                -> m (Event t a, a -> n b)
newEventWithFire trigger = do
  ref <- liftIO $ newIORef Nothing
  event <- newEventWithTrigger (\h -> writeIORef ref Nothing <$ writeIORef ref (Just h))
  return (event, \a -> trigger . F.foldMap (pure . (:=> a)) =<< liftIO (readIORef ref))

newExternalEvent :: MonadAppHost t m => m (Event t a, a -> IO Bool)
newExternalEvent = do
  asyncFire <- getAsyncFire
  newEventWithFire $ \triggers -> let l = DL.toList triggers in null l <$ asyncFire l

performEventAndTrigger_ :: MonadAppHost t m => Event t (AppPerformAction t) -> m ()
performEventAndTrigger_ event = performPostBuild_ $ pure (pure event, mempty)

performEvent_ :: MonadAppHost t m => Event t (HostFrame t ()) -> m ()
performEvent_ event = performEventAndTrigger_ $ fmap (mempty <$) event

performEvent :: MonadAppHost t m => Event t (HostFrame t a) -> m (Event t a)
performEvent event = do
  (result, fire) <- newEventWithFire return
  performEventAndTrigger_ $ (fire =<<) <$> event
  return result

test :: IO ()
test = runSpiderHost $ hostApp $ do
  asyncFire <- getAsyncFire
  (timer, fire) <- newEventWithFire $ asyncFire . DL.toList

  _ <- liftIO . forkIO $ forever $ do
    threadDelay 1000000
    putStrLn "[ext] timer!"
    fire ()
  time <- count timer
  let at3 = ffilter (== (3 :: Int)) $ updated time
  appDyn <- holdDyn (performEvent_ $ liftIO . print <$> updated time) $ ffor at3 $ \_ -> do
    performPostBuild_ $ mempty <$ liftIO (putStrLn "Built!")
    performEvent_ $ liftIO . putStrLn . ("switched: " ++) . show <$> updated time
  void $ performAppHost appDyn
