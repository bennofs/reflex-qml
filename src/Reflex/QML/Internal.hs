{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.QML.Internal where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
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

import qualified Data.DList as DL
import qualified Data.Foldable as F
import qualified Data.Traversable as T

data AppEnv t = AppEnv
  { envEventChan :: Chan [DSum (EventTrigger t)]
  }

data AppInfo t = AppInfo
  { eventsToPerform :: DL.DList (Event t (HostFrame t (DL.DList (DSum (EventTrigger t)))))
  , eventsToQuit :: DL.DList (Event t ())
  }
instance Monoid (AppInfo t) where
  mempty = AppInfo mempty mempty
  mappend (AppInfo a b) (AppInfo a' b') = AppInfo (mappend a a') (mappend b b')

newtype App t a = App
  { unAppT :: ReaderT (AppEnv t) (WriterT (AppInfo t) (HostFrame t)) a
  }
deriving instance ReflexHost t => Functor (App t)
deriving instance ReflexHost t => Applicative (App t)
deriving instance ReflexHost t => Monad (App t)
deriving instance ReflexHost t => MonadHold t (App t)
deriving instance ReflexHost t => MonadSample t (App t)
deriving instance ReflexHost t => MonadReflexCreateTrigger t (App t)
deriving instance (MonadIO (HostFrame t), ReflexHost t) => MonadIO (App t)

runAppT :: forall t m. (ReflexHost t, MonadIO m, MonadReflexHost t m) => App t () -> m ()
runAppT (App app) = do
  eventChannel <- liftIO newChan
  AppInfo{..} <- runHostFrame . execWriterT . runReaderT app $ AppEnv eventChannel
  nextActionEvent <- subscribeEvent $ mergeWith (liftA2 (<>)) $ DL.toList eventsToPerform
  quitEvent <- subscribeEvent $ mergeWith mappend $ DL.toList eventsToQuit

  let
    go [] = return ()
    go triggers = do
      (nextAction, continue) <- lift $ fireEventsAndRead triggers $
        (,) <$> eventValue nextActionEvent <*> fmap isNothing (readEvent quitEvent)
      guard continue
      maybe (return mempty) (lift . runHostFrame) nextAction >>= go . DL.toList

    eventValue :: forall t m a. MonadReadEvent t m => EventHandle t a -> m (Maybe a)
    eventValue = readEvent >=> T.sequenceA

  void . runMaybeT . forever $ do
    nextInput <- liftIO $ readChan eventChannel
    go nextInput
  return ()

class (ReflexHost t, MonadSample t m, MonadHold t m, MonadReflexCreateTrigger t m, MonadIO m, MonadIO (HostFrame t))
 => MonadApp t m | m -> t where
  triggerEvents :: [DSum (EventTrigger t)] -> m ()
  performEvent_ :: Event t (HostFrame t [DSum (EventTrigger t)]) -> m ()
  quitOn :: Event t () -> m ()

instance (Reflex t, ReflexHost t, MonadIO (HostFrame t)) => MonadApp t (App t) where
  triggerEvents triggers = App $ ask >>= \(AppEnv chan) -> liftIO $ writeChan chan triggers
  performEvent_ event = App $ tell $ mempty
    { eventsToPerform = pure $ fmap (fmap DL.fromList) event
    }
  quitOn event = App $ tell $ mempty { eventsToQuit = pure event }

newEventWithFire :: MonadApp t m => m (Event t a, a -> IO [DSum (EventTrigger t)])
newEventWithFire = do
  ref <- liftIO $ newIORef Nothing
  event <- newEventWithTrigger (\h -> writeIORef ref Nothing <$ writeIORef ref (Just h))
  return (event, \a -> F.toList . fmap (:=> a) <$> readIORef ref)

performEvent :: MonadApp t m => Event t (HostFrame t a) -> m (Event t a)
performEvent event = do
  (result, fire) <- newEventWithFire
  performEvent_ $ (liftIO . fire =<<) <$> event
  return result
