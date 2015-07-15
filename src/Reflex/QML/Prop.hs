{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

-- | Module for defining QML properties of objects.
--
-- Since the names in this module are very general, it is recommened that you import
-- it qualified:
--
-- @
--  import qualified Reflex.QML.Prop as Prop
-- @
--
-- Most functions in this module take the name of the property as the first argument
-- and the value as second argument.
module Reflex.QML.Prop
  ( constant
  , readonly
  , mutable
  , mutableHold
  , namespace
  , method
  , methodVoid
  , MethodSignature()
  ) where

import Control.Monad.Writer
import Data.IORef
import Graphics.QML.Marshal
import Graphics.QML.Objects
import Reflex hiding (constant)
import Reflex.Host.App
import Reflex.QML.Internal

-- | Add a constant property. A constant property never changes.
--
-- The advantage of using this function over just using 'readonly' with a constant dynamic
-- is that the QML engine will emit a warning if you listen for changes of a constant
-- property.
constant :: (Marshal a, CanReturnTo a ~ Yes, Monad m) => String -> Object m a -> ObjectBuilder m ()
constant name oval = do
  val <- lift $ registerObjectEvents oval
  tellDefM . pure . objMember $ defPropertyConst' name (\_ -> return val)

-- | Add a property that may be read from QML, but not written to from QML.
--
-- In contrast to 'constant', the property's value can still be changed by the
-- application itself though.
readonly :: (Marshal a, CanReturnTo a ~ Yes, MonadAppHost t m)
         => String -> Dynamic t (Object m a) -> ObjectBuilder m ()
readonly name ovalD = do
  sig <- liftIO newSignalKey
  valD <- lift $ dynRegisterObjectEvents ovalD
  tellDefM $ do
    currentRef <- liftIO . newIORef =<< sample (current valD)
    pure $ mconcat
      [ objMember $ defPropertySigRO' name sig (\_ -> readIORef currentRef)
      , objRegister $ \obj -> performEvent_ $ ffor (updated valD) $ \val -> liftIO $ do
          writeIORef currentRef val
          fireSignal sig obj
      ]

-- | Add a property that may be read and written from QML. Whenever the property is
-- changed *by QML code*, the returned event is fired. The event is not fired if the
-- property is changed from Haskell code (i.e. the passed Dynamic updated).
mutable :: (Marshal a, CanReturnTo a ~ Yes, CanGetFrom a ~ Yes, MonadAppHost t m)
        => String -> Dynamic t (Object m a) -> ObjectBuilder m (Event t a)
mutable name ovalD = do
  (event, fire) <- lift newExternalEvent
  sig <- liftIO newSignalKey
  valD <- lift $ dynRegisterObjectEvents ovalD
  tellDefM $ do
    ref <- liftIO . newIORef =<< sample (current valD)
    pure $ mconcat
      [ objMember $ defPropertySigRW' name sig (\_ -> readIORef ref) (const $ void . fire)
      , objRegister $ \obj -> performEvent_ $ ffor (updated valD) $ \val -> liftIO $ do
          writeIORef ref val
          fireSignal sig obj
      ]
  pure event

-- | A specialized version of 'mutable' that works like a shared variable: the second
-- argument is the initial value, and whenever the property is changed from QML, the
-- returned Dynamic is updated to that value.
--
-- This type of property may only be set from QML, and can never be changed by
-- Haskell code.
mutableHold :: (Marshal a, CanReturnTo a ~ Yes, CanGetFrom a ~ Yes, MonadAppHost t m)
            => String -> a -> ObjectBuilder m (Dynamic t a)
mutableHold name initial = mdo
  valD <- holdDyn initial changed
  changed <- mutable name =<< mapDyn pure valD
  pure valD

-- | Group all properties in an object builder under a common name. In QML, this is
-- represented by introducing a sub-object with the given name that contains all the
-- properties of the supplied object.
--
-- For example, if you have an object @o@ with members @x@, @y@, @z@, then
-- @namespace "app" o@ will produce an object where the properties are accessible from QML
-- as @app.x@, @app.y@ and @app.z@.
namespace :: MonadIO m => String -> ObjectBuilder m a -> ObjectBuilder m a
namespace name obj = do
  (ref, a) <- lift $ runObjectBuilder obj
  constant name ref
  return a

-- | Internal type for representing a QML method suffix, which executes some action based
-- on a value of type @a@ that is produced by perfoming some IO.
data AnyMethodSuffix a = forall ms. MethodSuffix ms => AnyMethodSuffix (IO a -> ms)

-- | Class of valid types for QML member methods.
-- See 'method' for documentation about what types are an instance of this class.
class MethodSignature a where
  -- | The first member of the result tuple of the function. A value of this type is
  -- not passed to QML, but only returned to Haskell.
  type MethodResultFst a :: *

  -- | The second member of the result tuple of the function. A value of this type is
  -- passed to QML. This type must therefore be returnable to QML, i.e. it must satisfy
  -- @CanReturnTo (MethodResultSnd a) ~ Yes@.
  type MethodResultSnd a :: *

  -- | Given a callback that is called with the result of the function and run whenever
  -- the method is called from QML, produce a method suffix that can be passed to
  -- 'defMethod' to create a QML method.
  methodInternal :: ((MethodResultFst a, MethodResultSnd a) -> IO ()) -> AnyMethodSuffix a

-- | Funtions are a valid method signature, provided that the result type of the function
-- is also an instance of 'MethodSignature'.
instance (MethodSignature b, Marshal a, CanGetFrom a ~ Yes) => MethodSignature (a -> b) where
  type MethodResultFst (a -> b) = MethodResultFst b
  type MethodResultSnd (a -> b) = MethodResultSnd b
  methodInternal callback = case methodInternal callback of
    AnyMethodSuffix ms -> AnyMethodSuffix $ \f x -> ms $ fmap ($ x) f

-- | Base case, a function of 0 arguments. It must return a tuple @IO (a,b)@ where @a@ is
-- the value to pass back to Haskell, and @b@ is the value to return to QML.
-- The function may also perform some IO.
instance (CanReturnTo b ~ Yes, Marshal b) => MethodSignature (IO (a,b)) where
  type MethodResultFst (IO (a,b)) = a
  type MethodResultSnd (IO (a,b)) = b
  methodInternal callback = AnyMethodSuffix $ join >=> \r -> snd r <$ callback r

-- | Add a QML method member to the object. The method can be called from QML, may do
-- some IO and then return a value.
--
-- The first argument is the name of the QML method. The second is a dynamic
-- that contains the current function to call when the QML method is called.
--
-- The @s@ type argument of this function represents the type of the QML member method.
-- It has to be of the form @a1 -> a2 -> ... -> aN -> IO (b,c)@.
-- @a1, a2, ..., aN@ must all be types that can be retrieved from QML
-- (i.e. for all N, @CanGetFrom aN ~ Yes@). Those are the arguments of the
-- method (there can also be zero arguments, in which case the type is just
-- @IO (b,c)@. Given the arguments, the method may then perform some IO
-- and return a result tuple. Only the value of type @c@ (second element of the tuple)
-- is used as a return value for the QML method, so it must be returnable to QML
-- (it must satisfy @CanReturnTo c ~ Yes@). Whenever the method is called, the
-- whole result tuple is provided through an Event to haskell though.
--
-- The separation is neccessary because you might want to return a value to
-- Haskell that is not serializable and so cannot be returned to QML. For
-- example, if you want to write a method that will open a file and then
-- store the Handle in a Dynamic, you need to return the file handle, but
-- that is not serializable.
method :: (MethodSignature s, MonadIO m, MonadAppHost t m)
       => String -> Dynamic t s -> ObjectBuilder m (Event t (MethodResultFst s, MethodResultSnd s))
method name funD = do
  (event, fire) <- lift newExternalEvent
  tellDefM $ do
    ref <- liftIO . newIORef =<< sample (current funD)
    let member = case methodInternal (void . fire) of
          AnyMethodSuffix ms -> defMethod' name $ \_ -> ms (readIORef ref)
    pure $ mconcat
      [ objMember member
      , objRegister (\_ -> performEvent_ $ liftIO . writeIORef ref <$> updated funD)
      ]
  return event

-- | Wrapper for 'method' for methods that don't return a value to QML.
-- All this does is ignoring the second element of the result event of 'method'.
methodVoid :: (MonadIO m, MonadAppHost t m, MethodSignature s)
           => String -> Dynamic t s -> ObjectBuilder m (Event t (MethodResultFst s))
methodVoid name funD = fmap fst <$> method name funD
