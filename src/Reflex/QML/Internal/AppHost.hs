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

-- | This is the environment in which the app host monad runs.
data AppEnv t = AppEnv
  { -- | This is the channel to which external events should push their triggers.
    --
    -- Because this is a channel, there is no guarrante that the event that was pushed
    -- is fired directly in the next frame, as there can already be other events waiting
    -- which will be fired first.
    envEventChan :: Chan [DSum (EventTrigger t)]
  }

-- | An action that is run after a frame. It may return event triggers to fire events.
--
type AppPerformAction t = HostFrame t (DL.DList (DSum (EventTrigger t)))

-- | Information required to set up the application.
data AppInfo t = AppInfo
  { -- | Events that are performed after each frame.
    --
    -- Each event in this list will be checked after a frame. If it is firing with some
    -- 'AppPerformAction', then this action will be executed.
    --
    -- The event triggers returned by each 'AppPerformAction' are collected in a list.
    -- If the list is non-empty, then a new frame will be created where all the collected
    -- triggers are fired at the same time. This process continues until no triggers are
    -- produced.
    --
    -- The returned triggers are fired immediately, even if the 'envEventChan' currently
    -- contains other triggers waiting to be fired. The events in the 'envEventChan' are
    -- only processed after all triggers have been fired and no new triggers were
    -- produced.
    --
    -- A common place where you need this is when you want to fire some event in response
    -- to another event, but you need to perform a monadic action to compute the value of
    -- the response. Using this field, you can perform the monadic action and then return
    -- a trigger to fire the event. This guarrantes that the event is fired immediately
    -- after the frame has finished, even if other, external events are waiting.
    eventsToPerform :: DL.DList (Event t (AppPerformAction t))

    -- | Events that, when fired, quit the application.
  , eventsToQuit :: DL.DList (Event t ())

    -- | Delayed triggers that will be fired immediately after the initial application
    -- setup has completed.
  , triggersToFire :: DL.DList (DSum (EventTrigger t))
  }

instance Monoid (AppInfo t) where
  mempty = AppInfo mempty mempty mempty
  mappend (AppInfo a b c) (AppInfo a' b' c') =
    AppInfo (mappend a a') (mappend b b') (mappend c c')

-- | Produce an 'AppInfo' which only contains 'eventsToPerform'. This is useful in a
-- monoid chain, like @infoToPerform toPerform <> infoToQuit toQuit@.
infoPerform :: DL.DList (Event t (AppPerformAction t)) -> AppInfo t
infoPerform x = mempty { eventsToPerform = x }

-- | Produce an 'AppInfo' which only contains 'eventsToQuit'.
infoQuit :: DL.DList (Event t ()) -> AppInfo t
infoQuit x = mempty { eventsToQuit = x }

-- | Produce an 'AppInfo' which only contains 'triggersToFire'.
infoToFire :: DL.DList (DSum (EventTrigger t)) -> AppInfo t
infoToFire x = mempty { triggersToFire = x }

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
  (a, Ap minfo) <- runWriterT . flip runReaderT env . unAppHost $ app
  (, a) <$> minfo

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

  void . runMaybeT $ do
    go $ DL.toList triggersToFire
    forever $ do
      nextInput <- liftIO . readChan $ envEventChan env
      go nextInput
  return ()

--------------------------------------------------------------------------------
class (ReflexHost t, MonadSample t m, MonadHold t m, MonadReflexCreateTrigger t m,
       MonadIO m, MonadIO (HostFrame t), MonadFix m, MonadFix (HostFrame t))
      => MonadAppHost t m | m -> t where
  getAsyncFire :: m ([DSum (EventTrigger t)] -> IO ())
  performPostBuild_ :: HostFrame t (AppInfo t) -> m ()
  dynAppHost :: Dynamic t (m a) -> m (Event t a)
  collectPostActions :: m a -> m (m (), a)

instance (ReflexHost t, MonadIO (HostFrame t)) => MonadAppHost t (AppHost t) where
  getAsyncFire = AppHost $ fmap liftIO . writeChan . envEventChan <$> ask

  performPostBuild_ mevent = AppHost . tell $ Ap mevent

  dynAppHost appDyn = do
    env <- AppHost ask
    (updatedInfo, revent) <- fmap splitE . performEvent $
      runAppHostFrame env <$> updated appDyn
    performPostBuild_ $ do
      initialInfo <- execAppHostFrame env =<< sample (current appDyn)
      switchAppInfo initialInfo updatedInfo
    return revent

  collectPostActions (AppHost app) = do
    env <- AppHost ask
    (r, minfo) <- AppHost . lift . lift . runWriterT $ runReaderT app env
    return (AppHost $ tell minfo, r)

appInfoEvents :: (Reflex t, Applicative (HostFrame t))
              => AppInfo t -> (Event t (AppPerformAction t), Event t ())
appInfoEvents AppInfo{..} =
  ( mergeWith (liftA2 (<>)) $ DL.toList eventsToPerform
  , leftmost $ DL.toList eventsToQuit
  )

switchAppInfo :: (ReflexHost t, MonadHold t m, Reflex t)
              => AppInfo t -> Event t (AppInfo t) -> m (AppInfo t)
switchAppInfo initialInfo updatedInfo = do
  toPerform <- switch <$> hold initialToPerform updatedToPerform
  toQuit    <- switch <$> hold initialToQuit updatedToQuit
  pure $ AppInfo
    { eventsToPerform = pure toPerform <> pure (pure . triggersToFire <$> updatedInfo)
    , eventsToQuit = pure toQuit
    , triggersToFire = triggersToFire initialInfo
    }
 where
  (updatedToPerform, updatedToQuit) = splitE $ fmap appInfoEvents updatedInfo
  (initialToPerform, initialToQuit) = appInfoEvents initialInfo

--------------------------------------------------------------------------------
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
performEventAndTrigger_ event = performPostBuild_ $ pure mempty { eventsToPerform = pure event }

performEvent_ :: MonadAppHost t m => Event t (HostFrame t ()) -> m ()
performEvent_ event = performEventAndTrigger_ $ fmap (mempty <$) event

performEvent :: MonadAppHost t m => Event t (HostFrame t a) -> m (Event t a)
performEvent event = do
  (result, fire) <- newEventWithFire return
  performEventAndTrigger_ $ (fire =<<) <$> event
  return result

holdAppHost :: MonadAppHost t m => m a -> Event t (m a) -> m (Dynamic t a)
holdAppHost mInit mChanged = do
  (applyPostActions, aInit) <- collectPostActions mInit
  aChanged <- dynAppHost =<< holdDyn (aInit <$ applyPostActions) mChanged
  holdDyn aInit aChanged

test :: IO ()
test = runSpiderHost $ hostApp $ liftIO (putStrLn "before") >> performPostBuild_ (liftIO $ mempty <$ putStrLn "postBuild!") >> liftIO (putStrLn "after")
