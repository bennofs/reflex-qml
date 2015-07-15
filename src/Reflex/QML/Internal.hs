{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-- | This is the internal module, which exports all functions used to implement this
-- library. There is no guarrante that functions exported by this module will be
-- available in the next version.
-- If possible, use just thn 'Reflex.QML' module.
module Reflex.QML.Internal where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Writer
import Data.Semigroup.Applicative
import Data.Typeable
import Graphics.QML
import Reflex
import Reflex.Host.App
import Reflex.Host.Class

import qualified Data.DList as DL

import Prelude -- to silence AMP warnings
--------------------------------------------------------------------------------

-- | A description of a concrete QML object. Collects all information needed to create
-- an object.
data ObjectDef m = ObjectDef
  { -- | Function run after the object is created-
    register :: ObjRef () -> m ()

    -- | The QML members that this object consists of.
  , members  :: DL.DList (Member ())
  }

-- | Create an object definition with just this single member.
-- This is typically used in conjunction with the Monoid instance, like this:
-- @objMember member <> objRegister register <> ...@
objMember :: Applicative m => Member () -> ObjectDef m
objMember m = mempty { members = pure m }

-- | Add a hook that is run after the object is created.
-- This function may be used to fire signals on the object in
-- response to FRP events.
objRegister :: (ObjRef () -> m ()) -> ObjectDef m
objRegister r = ObjectDef r mempty

-- | Multiple objects can be merged, creating an object that
-- consists of the the members of both objects.
instance Applicative m => Monoid (ObjectDef m) where
  mempty = ObjectDef (const $ pure ()) mempty
  mappend (ObjectDef r m) (ObjectDef r' m') = ObjectDef (liftA2 (*>) r r') (mappend m m')

-- | A monad transformer for building a QML object.
--
-- In this monad, you can use all the functions of the 'Reflex.QML.Prop' module
-- to add properties to the object. This will build a description of the QML object.
--
-- This description can then be converted to a QML object using either 'runObjectBuilder' or
-- 'buildObject'.
newtype ObjectBuilder m a = ObjectBuilder { unObjectT :: WriterT (Ap m (ObjectDef m)) m a }
  deriving (Functor, Applicative, Monad, MonadHold t, MonadSample t,
            MonadReflexCreateTrigger t, MonadFix, MonadIO)

-- | 'ObjectBuilder' is a monad transformer, so you can still run actions in the
-- base monad using 'lift'.
instance MonadTrans ObjectBuilder where
  lift = ObjectBuilder . lift

-- | A context in which you may operate with QML objects.
--
-- 'runObjectBuilder' cannot give you a raw QML object reference, since objects
-- also need to keep track of the FRP events that they depend on (for example,
-- if your object has a @'Reflex.QML.Prop.readonly' someDyn@ member, then the object
-- depends on @someDyn@). So instead of giving you a raw object reference, the
-- function instead returns the reference wrapped in the 'Object' monad, which keeps
-- track of these dependencies.
--
-- Since this type is a Monad, if you have a list of these contexts, you can collapse
-- them into a single context: @'sequence' :: Monad m => [Object m a] -> Object m [a]@.
newtype Object m a = Object (Writer (Traversal m) a)
  deriving (Functor, Applicative, Monad, MonadFix)
--------------------------------------------------------------------------------

-- | Add another object description to the current object. When running the builder,
-- all object descriptions are merged using the 'Monoid' instance of 'ObjectDef'.
tellDefM :: Monad m => m (ObjectDef m) -> ObjectBuilder m ()
tellDefM = ObjectBuilder . tell . Ap

-- | Create a new QML object from a description. The function does not directly
-- return a reference to the QML object, but instead wraps in an 'Object' context.
runObjectBuilder :: MonadIO m => ObjectBuilder m a -> m (Object m AnyObjRef, a)
runObjectBuilder (ObjectBuilder m) = do
  (a, ObjectDef{..}) <- traverse getApp =<< runWriterT m
  obj <- liftIO $ newClass (DL.toList members) >>= flip newObject ()
  return (Object $ anyObjRef obj <$ tell (Traversal $ register obj), a)

-- | This is just like 'runObjectBuilder', except that it ignores the return value
-- of the 'ObjectBuilder' and only returns the object itself.
buildObject :: MonadIO m => ObjectBuilder m () -> m (Object m AnyObjRef)
buildObject = fmap fst . runObjectBuilder

-- | Register the events for the objects contained in this object context. This
-- will add all the 'eventsToPerform' that the objects in this context require.
registerObjectEvents :: Functor m => Object m a -> m a
registerObjectEvents = uncurry (flip (<$)) . unsafeRunObject

-- | Like 'registerObjectEvents', but only registers the events for the object context
-- that is currently contained in the dynamic. After the object context is switched out,
-- the events for this context will no longer be registered.
dynRegisterObjectEvents :: MonadAppHost t m => Dynamic t (Object m a) -> m (Dynamic t a)
dynRegisterObjectEvents dyn = do
  (dynRegister, dynVal) <- splitDyn =<< mapDyn unsafeRunObject dyn
  void $ dynAppHost dynRegister
  pure dynVal

-- | Run an object context, returning the value inside it together with an action that
-- registers the events.
---
-- This is unsafe, since the value may be used without registering the object, which
-- might lead to lost updates (because the members of the objects inside the object
-- context may depend on FRP events/behaviors, which aren't listened to as long as
-- the object is not registered).
unsafeRunObject :: Object m a -> (m (), a)
unsafeRunObject (Object m) = (registerAction, a)
  where (a, Traversal registerAction) = runWriter m
--------------------------------------------------------------------------------

-- | This exception is used internally to terminate the QML engine when the
-- application quits.
data ApplicationExited = ApplicationExited deriving (Show, Typeable)

-- | Make this an Exception. No special things needed, the default instance
-- works for us.
instance Exception ApplicationExited

-- | Handle the exit exception. This function will simply ignore the exception.
-- This is correct, since the only purpose of this exception is to interrupt the
-- QML engine.
handleExitException :: ApplicationExited -> IO ()
handleExitException ApplicationExited = return ()

-- | Run a QML application using reflex. The 'EngineConfig' specifies options such
-- as the main QML document to load. The supplied 'ObjectBuilder' will be used to
-- build the root context object of the application.
--
-- Note that the field 'contextObject' of the engine config is overriden by this
-- function, so setting that field has no effect.
hostQmlApp :: EngineConfig -> (forall t m. MonadAppHost t m => ObjectBuilder m ()) -> RunQML ()
hostQmlApp config app = liftIO . handle handleExitException $ do
  -- This variable will hold the context object as soon as the reflex thread
  -- has initialized itself
  objVar <- newEmptyMVar

  -- Remember the thread id of the main thread, to kill it when we want to exit
  mainThreadId <- myThreadId

  -- The reflex thread. Since running the QML engine blocks the main thread
  -- (we can't run use runEngineAsync because then we don't know when to stop),
  -- a thread is used to process the reflex events.
  --
  -- We also don't want to run the QML application in a thread, since at least
  -- on some platforms, it must run on the main thread.
  let reflexThread = do
        runSpiderHost $ hostApp $
          buildObject app >>= registerObjectEvents >>= liftIO . putMVar objVar
        throwTo mainThreadId ApplicationExited

  -- Now start the reflex thread and run the QML engine in the main thread after the
  -- reflex thread has initialized itself far enough to produce the context object.
  withAsync reflexThread . const $ do
    obj <- takeMVar objVar
    void . requireEventLoop $ runEngine config
      { contextObject = Just obj
      }
