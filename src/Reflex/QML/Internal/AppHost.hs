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
-- For more information about this type, see the field 'eventsToPerform' of 'AppInfo'.
type AppPerformAction t = HostFrame t (DL.DList (DSum (EventTrigger t)))

-- | Information required to set up the application. This also contains all reflex events
-- that the application wants to perform.
data AppInfo t = AppInfo
  { -- | Events that are performed after each frame.
    --
    -- Each event in this list will be checked after a frame. If it is firing with some
    -- 'AppPerformAction', then this action will be executed. The events will be fired
    -- before any other, currently waiting external events contained in the
    -- `envEventChan` are processed.
    --
    -- The event occurrences returned by each 'AppPerformAction' are collected in a list.
    -- If the list is non-empty, then a new frame will be created where all the collected
    -- occurrences are fired at the same time. This process continues until no events
    -- should be fired anymore. Only after this process has finished, external events
    -- contained in the `envEventChan` are processed again.
    --
    -- A common place where you need this is when you want to fire some event in response
    -- to another event, but you need to perform a monadic action such as IO to compute
    -- the value of the response. Using this field, you can perform the monadic action
    -- and then return a trigger to fire the event. This guarrantes that the event is
    -- fired immediately after the frame has finished, even if other, external events
    -- are waiting.
    eventsToPerform :: DL.DList (Event t (AppPerformAction t))

    -- | Events that, when fired, quit the application.
  , eventsToQuit :: DL.DList (Event t ())

    -- | Delayed event triggers that will be fired immediately after the initial
    -- application setup has completed, before any external events are processed.
  , triggersToFire :: Ap (HostFrame t) (DL.DList (DSum (EventTrigger t)))
  }

instance Applicative (HostFrame t) => Monoid (AppInfo t) where
  mempty = AppInfo mempty mempty mempty
  mappend (AppInfo a b c) (AppInfo a' b' c') =
    AppInfo (mappend a a') (mappend b b') (mappend c c')

-- | Produce an 'AppInfo' which only contains 'eventsToPerform'. This is useful in a
-- monoid chain, like @infoToPerform toPerform <> infoToQuit toQuit@.
infoPerform :: Applicative (HostFrame t)
            => DL.DList (Event t (AppPerformAction t)) -> AppInfo t
infoPerform x = mempty { eventsToPerform = x }

-- | Produce an 'AppInfo' which only contains 'eventsToQuit'.
infoQuit :: Applicative (HostFrame t) => DL.DList (Event t ()) -> AppInfo t
infoQuit x = mempty { eventsToQuit = x }

-- | Produce an 'AppInfo' which only contains 'triggersToFire'.
infoFire :: Applicative (HostFrame t)
           => HostFrame t (DL.DList (DSum (EventTrigger t))) -> AppInfo t
infoFire x = mempty { triggersToFire = Ap x }

-- | Extract the 'eventsToPerform' and 'eventsToQuit' and merge each into a single event.
appInfoEvents :: (Reflex t, Applicative (HostFrame t))
              => AppInfo t -> (Event t (AppPerformAction t), Event t ())
appInfoEvents AppInfo{..} =
  ( mergeWith (liftA2 (<>)) $ DL.toList eventsToPerform
  , leftmost $ DL.toList eventsToQuit
  )

-- | Switch to a different 'AppInfo' whenever an 'Event' fires. Only the events of the
-- currently active application are performed.
--
-- This low-level primitive is used for implementing higher-level functions such as
-- 'switchAppHost', 'performAppHost' or 'dynAppHost'.
switchAppInfo :: (ReflexHost t, MonadHold t m, Reflex t)
              => AppInfo t -> Event t (AppInfo t) -> m (AppInfo t)
switchAppInfo initialInfo updatedInfo = do
  toPerform <- switch <$> hold initialToPerform updatedToPerform
  toQuit    <- switch <$> hold initialToQuit updatedToQuit
  pure $ AppInfo
    { eventsToPerform = pure toPerform <> pure (getApp . triggersToFire <$> updatedInfo)
    , eventsToQuit = pure toQuit
    , triggersToFire = triggersToFire initialInfo
    }
 where
  (updatedToPerform, updatedToQuit) = splitE $ fmap appInfoEvents updatedInfo
  (initialToPerform, initialToQuit) = appInfoEvents initialInfo
--------------------------------------------------------------------------------

-- | An implementation of the 'MonadAppHost' typeclass. You should not need to use this
-- type directly. Instead, use the methods provided by the 'MonadAppHost' typeclass and
-- then run your application using 'hostApp' to choose this implementation.
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

-- | Run the application host monad in a reflex host frame and return the produced
-- application info.
execAppHostFrame :: ReflexHost t => AppEnv t -> AppHost t () -> HostFrame t (AppInfo t)
execAppHostFrame env app = do
  Ap minfo <- execWriterT . flip runReaderT env . unAppHost $ app
  minfo

-- | Run the 'AppHost' monad. This function will block until the application exits (by
-- firing one of the 'eventsToQuit').
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
    go =<< lift (runHostFrame (DL.toList <$> getApp triggersToFire))
    forever $ do
      nextInput <- liftIO . readChan $ envEventChan env
      go nextInput
  return ()

--------------------------------------------------------------------------------

-- | Class providing common functionality for implementing reflex frameworks.
--
-- The host monad is used to setup events from external sources (such as user input) and
-- execute actions in response to events (such as performing some IO). This is class
-- encapsulating common functionality of such a monad. An implementation is the 'AppHost'
-- monad.
--
-- Much of the functionality of this class is also provided by its superclasses.
class (ReflexHost t, MonadSample t m, MonadHold t m, MonadReflexCreateTrigger t m,
       MonadIO m, MonadIO (HostFrame t), MonadFix m, MonadFix (HostFrame t))
      => MonadAppHost t m | m -> t where
  -- | Primitive function to create events from external sources.
  --
  -- In reflex, when you create an event (using 'newEventWithTrigger' for example),
  -- you get passed an 'EventTrigger t'. This action returns a function which, given
  -- a trigger and a value for an event, can fire the event. It takes a list of triggers
  -- with values, so you can also use it to fire multiple events in parallel.
  --
  -- Note that the events fired by this function are fired asynchronously. In particular,
  -- if a lot of events are fired, then it can happen that the event queue already
  -- contains other events. In that case, those events will be fired first.
  getAsyncFire :: m ([DSum (EventTrigger t)] -> IO ())

  -- | Get a function to run the host monad. Useful for implementing dynamic switching.
  --
  -- Running the host monad performs 3 steps:
  --
  -- 1. First, the events and behaviors (using hold) are created. This step does not read
  --    the value of any behavior, since that breaks MonadFix in some cases.
  -- 2. After all events and behaviors have been created, the initial value of behavior
  --    can now be read (using for example 'sample')
  -- 3. This information is then used to create an 'AppInfo' which contains all the
  --    information about the actions to perform in response to the FRP events.
  --
  -- This is why the type of the @run@ function returned by this action is
  -- @m a -> HostFrame t (HostFrame t (AppInfo t), a)@.
  -- Executing outermost @HostFrame t@ will only perform step 1. The inner layer will
  -- then perform step 2, and the returned 'AppInfo' represents step 3.
  getRunAppHost :: m (m a -> HostFrame t (HostFrame t (AppInfo t), a))

  -- | Run an action after all other actions have been ran and add information about the
  -- application. After the host monad's actions have been executed, actions registered
  -- with this function will be ran and the returned 'AppInfo's will be merged to get the
  -- final 'AppInfo'.
  --
  -- One use case for this function is to sample the initial values of some behaviors.
  -- This cannot be done directly in the host monad, since that would break MonadFix in
  -- some cases, since it is not lazy enough. Using this function, the sampling can
  -- instead be done after the host monad has finished, so the behavior is not forced too
  -- early.
  performPostBuild_ :: HostFrame t (AppInfo t) -> m ()

  -- | Directly run a HostFrame action in the host app monad.
  liftHostFrame :: HostFrame t a -> m a

instance (ReflexHost t, MonadIO (HostFrame t)) => MonadAppHost t (AppHost t) where
  getAsyncFire = AppHost $ fmap liftIO . writeChan . envEventChan <$> ask
  getRunAppHost = AppHost $ do
    env <- ask
    let rearrange (a, Ap m) = (m, a)
    pure $ fmap rearrange . runWriterT . flip runReaderT env . unAppHost
  performPostBuild_ mevent = AppHost . tell $ Ap mevent
  liftHostFrame = AppHost . lift . lift

--------------------------------------------------------------------------------

-- | Create a new event and return a function that can be used to construct an event
-- trigger with an associated value. Note that this by itself will not fire the event.
-- To fire the event, you still need to use either 'performPostBuild_' or 'getAsyncFire'
-- which can fire these event triggers with an associated value.
--
-- Note that in some cases (such as when there are no listeners), the returned function
-- does return 'Nothing' instead of an event trigger. This does not mean that it will
-- neccessarily return Nothing on the next call too though.
newEventWithConstructor
  :: MonadAppHost t m => m (Event t a, a -> IO (Maybe (DSum (EventTrigger t))))
newEventWithConstructor = do
  ref <- liftIO $ newIORef Nothing
  event <- newEventWithTrigger (\h -> writeIORef ref Nothing <$ writeIORef ref (Just h))
  return (event, \a -> fmap (:=> a) <$> liftIO (readIORef ref))

-- | Create a new event from an external event source. The returned function can be used
-- to fire the event.
newExternalEvent :: MonadAppHost t m => m (Event t a, a -> IO Bool)
newExternalEvent = do
  asyncFire <- getAsyncFire
  (event, construct) <- newEventWithConstructor
  return (event, fmap isJust . traverse (asyncFire . pure) <=< construct)

-- | Run a monadic action after each frame in which the given event fires
-- and possibly fire other events as a result (fired in a new frame, together with all
-- other events to fire from this frame).
--
-- The events are fired synchronously, so they are processed before any other
-- external events.
performEventAndTrigger_ :: MonadAppHost t m => Event t (AppPerformAction t) -> m ()
performEventAndTrigger_ = performPostBuild_ . pure . infoPerform . pure

-- | Run a monadic action after each frame in which the  event fires.
performEvent_ :: MonadAppHost t m => Event t (HostFrame t ()) -> m ()
performEvent_ = performEventAndTrigger_ . fmap (mempty <$)

-- | Run a monadic action after each frame in which the event fires, and return the result
-- in a new event which is fired immediately following the frame in which the original
-- event fired.
performEvent :: MonadAppHost t m => Event t (HostFrame t a) -> m (Event t a)
performEvent event = do
  (result, construct) <- newEventWithConstructor
  performEventAndTrigger_ $ (fmap (F.foldMap pure) . liftIO . construct =<<) <$> event
  return result

-- | Run an action in a 'MonadAppHost' monad, but do not register the 'AppInfo' for this
-- action nor its postBuild actions.
-- Instead, the 'AppInfo' for this action is collected and returned.
--
-- For example, all 'performEvent_' calls inside the passed action will not actually be
-- performed, as long as the returned 'AppInfo' is not registered manually.
runAppHost :: MonadAppHost t m => m a -> m (HostFrame t (AppInfo t), a)
runAppHost action = liftHostFrame . ($ action) =<< getRunAppHost

-- | Switch to a different host action after an event fires. Only the 'AppInfo' of the
-- currently active application is registered. For example, 'performEvent' calls are only
-- executed for the currently active application. As soon as it is switched out and
-- replaced by a different application, they are no longer executed.
--
-- The first argument specifies the postBuild action that is used initially, before the
-- event fires the first time.
--
-- Whenever a switch to a new host action happens, the returned event is fired in the
-- next frame with the result of running it.
switchAppHost :: MonadAppHost t m => HostFrame t (AppInfo t) -> Event t (m a) -> m (Event t a)
switchAppHost initial event = do
  run <- getRunAppHost
  let runWithPost = run >=> \(post, a) -> (,a) <$> post
  (infoEvent, valueEvent) <- fmap splitE . performEvent $ runWithPost <$> event
  performPostBuild_ $ flip switchAppInfo infoEvent =<< initial
  return valueEvent

-- | Like 'switchAppHost', but without an initial action.
performAppHost :: MonadAppHost t m => Event t (m a) -> m (Event t a)
performAppHost = switchAppHost (pure mempty)

-- | Like 'switchAppHost', but taking the initial action from a 'Dynamic'. The returned
-- event is also fired once in the frame directly after the call to this function, firing
-- with the result of the initial action.
dynAppHost :: MonadAppHost t m => Dynamic t (m a) -> m (Event t a)
dynAppHost dyn = do
  run <- getRunAppHost
  (initialEvent, initialConstruct) <- newEventWithConstructor
  updatedEvent <- flip switchAppHost (updated dyn) $ do
    (minfo, r) <- sample (current dyn) >>= run
    info <- minfo
    trigger <- liftIO $ initialConstruct r
    pure $ info <> F.foldMap (infoFire . pure . pure) trigger
  pure $ leftmost [updatedEvent, initialEvent]

-- | Like 'switchAppHost', but taking the initial postBuild action from another host
-- action.
holdAppHost :: MonadAppHost t m => m a -> Event t (m a) -> m (Dynamic t a)
holdAppHost mInit mChanged = do
  (postActions, aInit) <- runAppHost mInit
  aChanged <- switchAppHost postActions mChanged
  holdDyn aInit aChanged
