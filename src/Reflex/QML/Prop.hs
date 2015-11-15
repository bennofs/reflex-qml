{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  ( constant, constantP
  , readonly, readonlyP
  , mutable, mutableP
  , mutableHold
  , namespace
  , method, methodConst, methodVoid
  , SimpleResult(), simpleResult, result
  , HaskellResult(), haskellResult, haskellResultIO, noResult
  , AnnotatedResult(), annotatedResult
  , MethodSignature(), MethodResult
  ) where

import Control.Monad
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

-- | Shorthand for a 'constant' property that only contains primitive marshallable values,
-- and doesn't contain any objects that may generate signals.
constantP :: (Marshal a, CanReturnTo a ~ Yes, Monad m) => String -> a -> ObjectBuilder m ()
constantP name = constant name . pure

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

-- | Shorthand for a 'readonly' property that doesn't contain any objects, so it only
-- consists of a primitive marshallable value.
readonlyP :: (Marshal a, CanReturnTo a ~ Yes, MonadAppHost t m)
          => String -> Dynamic t a -> ObjectBuilder m ()
readonlyP name = readonly name <=< mapDyn pure

-- | Add a property that may be read and written from QML. Whenever the property is
-- changed /by QML code/, the returned event is fired. The event is not fired if the
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

-- | Shorthand for 'mutable' property only containing primitive marshallable values
-- which cannot contain objects that generate events themselves.
mutableP :: (Marshal a, CanReturnTo a ~ Yes, CanGetFrom a ~ Yes, MonadAppHost t m)
         => String -> Dynamic t a -> ObjectBuilder m (Event t a)
mutableP name = mutable name <=< mapDyn pure

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
  -- | The result type of the method. This is the type that is passed back to Haskell.
  -- The type of the result returned to QML may be different.
  type MethodResult a :: *

  -- | Given a callback that is called with the result of the function and run whenever
  -- the method is called from QML, produce a method suffix that can be passed to
  -- 'defMethod' to create a QML method.
  methodInternal :: (MethodResult a -> IO ()) -> AnyMethodSuffix a

-- | Funtions are a valid method signature, provided that the result type of the function
-- is also an instance of 'MethodSignature'.
instance (MethodSignature b, Marshal a, CanGetFrom a ~ Yes) => MethodSignature (a -> b) where
  type MethodResult (a -> b) = MethodResult b
  methodInternal callback = case methodInternal callback of
    AnyMethodSuffix ms -> AnyMethodSuffix $ \f x -> ms $ fmap ($ x) f

-- | An annnotated result. This result type consists of two parts, one part is returned
-- to Haskell only and the other part is returned to both Haskell and QML.
-- The Haskell result will be a tuple @(r,a)@.
--
-- This can be used to return non-serializable values from method calls to Haskell.
newtype AnnotatedResult r a = AnnotatedResult { unAnnotatedResult :: IO (r,a) }

-- | A simple result, where the same value is returned to Haskell as is returned to QML.
newtype SimpleResult a = SimpleResult { unSimpleResult :: IO a }
  deriving (Functor, Applicative, Monad)

-- | A void result, only returning a value to Haskell but nothing to QML.
newtype HaskellResult a = HaskellResult { unHaskellResult :: IO a }
  deriving (Functor, Applicative, Monad)

-- | Return an annotated result. The first element of the tuple is the annotation, while
-- the second element is the actual result returned by the QML method call.
annotatedResult :: (Marshal a, CanReturnTo a ~ Yes) => IO (r,a) -> AnnotatedResult r a
annotatedResult = AnnotatedResult

-- | Build a simple return value. The argument is an IO action that produces the value
-- returned by the QML method call.
simpleResult :: (Marshal a, CanReturnTo a ~ Yes) => IO a -> SimpleResult a
simpleResult = SimpleResult

-- | Build a simple return value from a pure value, without running any IO.
result :: (Marshal a, CanReturnTo a ~ Yes) => a -> SimpleResult a
result = SimpleResult . pure

-- | Return nothing to QML, but pass a value back to haskell.
haskellResult :: a -> HaskellResult a
haskellResult = HaskellResult . pure

-- | Like 'haskellResult', but also allows you to perform an IO action to compute
-- the value.
haskellResultIO :: IO a -> HaskellResult a
haskellResultIO = HaskellResult

-- | Return nothing at all, neither to Haskell nor to QML.
noResult :: HaskellResult ()
noResult = haskellResult ()

-- | A function of 0 arguments that just returns an annotated result is a valid method
-- signature.
instance (CanReturnTo b ~ Yes, Marshal b) => MethodSignature (AnnotatedResult a b) where
  type MethodResult (AnnotatedResult a b) = (a,b)
  methodInternal callback = AnyMethodSuffix $
    (>>= unAnnotatedResult) >=> \r -> snd r <$ callback r

-- | A function of 0 arguments that just returns a simple result is a valid method
-- signature.
instance (CanReturnTo b ~ Yes, Marshal b) => MethodSignature (SimpleResult b) where
  type MethodResult (SimpleResult b) = b
  methodInternal callback = AnyMethodSuffix $
    (>>= unSimpleResult) >=> \r -> r <$ callback r

-- | A function of 0 arguments that just returns a void result is a valid method
-- signature.
instance MethodSignature (HaskellResult a) where
  type MethodResult (HaskellResult a) = a
  methodInternal callback = AnyMethodSuffix $ (>>= unHaskellResult) >=> callback

-- | Add a QML method member to the object. The method can be called from QML, may do
-- some IO and then return a value.
--
-- The first argument is the name of the QML method. The second is a dynamic
-- that contains the current function to call when the QML method is called.
--
-- The @s@ type argument of this function represents the type of the QML member method.
-- It has to be of the form @a1 -> a2 -> ... -> aN -> type (b,c)@, where type is either
-- 'SimpleResult', 'HaskellResult' or 'AnnotatedResult'.
-- @a1, a2, ..., aN@ must all be types that can be retrieved from QML
-- (i.e. for all N, @CanGetFrom aN ~ Yes@). Those are the arguments of the
-- method (there can also be zero arguments, in which case the type is just
-- @type (b,c)@.
--
-- __Example types__
--
-- @
-- -- The QML method will have no return value (type: Int -> ())
-- 'method' :: ('MonadIO' m, 'MonadAppHost' t m) => 'String' -> 'Dynamic' t ('Int' -> 'HaskellResult' 'Int') -> 'ObjectBuilder' m ('Event' t 'Int')
--
-- -- The QML method will return an Int (type: Int -> Int)
-- 'method' :: ('MonadIO' m, 'MonadAppHost' t m) => 'String' -> 'Dynamic' t ('Int' -> 'SimpleResult' 'Int') -> 'ObjectBuilder' m ('Event' t 'Int')
--
-- -- The QML method will only return an Int (type: Int -> Int)
-- 'method' :: ('MonadIO' m, 'MonadAppHost' t m) => 'String' -> 'Dynamic' t (Int -> 'AnnotatedResult' ['String'] 'Int') -> 'ObjectBuilder' m ('Event' t (['String'], 'Int'))
-- @
--
-- __Example usage__
--
-- @
-- -- From todomcv: clearCompletedE is fired whenever the method is called in QML
-- clearCompletedE <- 'Prop.method' "clearCompleted" . 'constDyn' $ 'Prop.noResult'
--
-- -- From todomvc: "newItem" is a QML function that takes a String argument (represented as Text in Haskell.) and has no return value.
-- -- Whenever newItem is called in QML, the newItem event fires with the corresponding argument.
-- newItem \<- 'Prop.method' "newItem" . 'constDyn' $ \\desc -\> 'Prop.haskellResult' $ return desc
-- @
method :: (MethodSignature s, MonadIO m, MonadAppHost t m)
       => String -> Dynamic t s -> ObjectBuilder m (Event t (MethodResult s))
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

-- | Like 'method', but doesn't support changing the method action.
methodConst :: (MethodSignature s, MonadIO m, MonadAppHost t m)
            => String -> s -> ObjectBuilder m (Event t (MethodResult s))
methodConst name = method name . constDyn

-- | Construct a 'method' without any arguments and no return value.
methodVoid :: (MonadIO m, MonadAppHost t m) => String -> ObjectBuilder m (Event t ())
methodVoid name = methodConst name noResult
