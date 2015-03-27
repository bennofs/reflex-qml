{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- | Reactive-banana based interface to HsQML
module Graphics.QML.React
  ( -- * Running
    runQMLReact 

    -- * Objects
  , QmlObject()
  , objectRef
  , finalObject
  , qmlObject
  , QObject(..)
  , imapQObject
  , forQObject_
  , QObjectDerivingError

    -- * Members
  , Member()
  , View()
  , Mut()
  , changed
  , Static()
  , static
  , Def(), DefWith(), Final()
  , AsProperty(..)
  , HasBehavior(..)
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad.Identity
import Control.Monad.Writer hiding (Product)
import Data.IORef
import Data.Traversable (Traversable(traverse))
import GHC.Generics
import GHC.TypeLits
import Reactive.Banana hiding (Identity)
import Reactive.Banana.Frameworks

import qualified Graphics.QML as Qml
--------------------------------------------------------------------------------

-- | Compiles the supplied reactive-banana network, 'actuate's it and runs the QML engine
-- with the specified engine config (warning: the context object is overriden by
-- this function). This function will not return until the engine has terminated.
runQMLReact :: Qml.EngineConfig -> (forall t. Frameworks t => Moment t (QmlObject t o)) -> IO ()
runQMLReact config networkDefinition = do
  objVar <- newEmptyMVar
  network <- compile (networkDefinition >>= liftIO . putMVar objVar . objectRef)
  obj <- takeMVar objVar
  actuate network
  Qml.runEngineLoop config { Qml.contextObject = Just obj }
  pause network

-- | A reference to a QML object. Values of this data type can be constructed with
-- 'qmlObject'.
data QmlObject t o = QmlObject (Qml.ObjRef ()) (o (Final t))

instance Qml.Marshal (QmlObject t o) where
  type MarshalMode (QmlObject t o) c d = Qml.ModeObjTo () c
  marshaller = Qml.toMarshaller $ \(QmlObject o _) -> o

-- | Return the final object. The final object contains the values, signals and behaviors
-- (depending on the member kind) for all members of the object.
finalObject :: QmlObject t o -> o (Final t)
finalObject (QmlObject _ o) = o

-- | Return the reference to the underlying QML object.
objectRef :: QmlObject t o -> Qml.AnyObjRef
objectRef (QmlObject r _) = Qml.anyObjRef r

-- | Construct a new 'QmlObject' from an object definition.
--
-- An object type is a data type with one constructor and fields containing the
-- 'Member's of the object. The object type must be an instance of 'QObject'.
--
-- For example, here is an object with a viewable integer @value@ property
-- (viewable properties can only be read from QML, not modified) and a mutable integer
-- @input@ property (mutable properties can be modified from QML).
--
-- @
-- data Foo p = Foo
--   { value :: 'Member' 'View' Int p
--   , input :: 'Member' 'Mut'  Int p
--   } deriving 'Generic1'
-- instance 'QObject' Foo
-- @
--
-- The object type has one type argument @p@ which the library requires so that object
-- types can be re-used for different purposes. It is called object type tag.
-- To create an instance of the object type, we now need to supply a definition for the
-- members, which has type @Def t Foo@ (the @t@ is the reactive-banana time
-- parameter):
--
-- @
-- -- 'Def' t 'Foo' is a type synonym synonym for 'Foo' ('DefWith' t 'Foo')
-- definition :: Frameworks t => Moment t ('Def' t 'Foo')
-- definition = Foo
--   \<$\> 'prop' (\\self -> accumB 0 (fmap (+) ('changed' $ input self)))
--   \<*\> 'prop' (\\self -> pure 0)
-- @
--
-- We supply a definition for each member. Note how we get access to a @self@ argument,
-- so that properties can refer to each other.
-- This definition can then be passed to 'qmlObject' to create a new object reference.
qmlObject :: forall o t. (QObject o, Frameworks t) => Def t o -> Moment t (QmlObject t o)
qmlObject o = do
  let fixed = imapQObject fixMember o

      fixMember :: String -> Member k a (DefWith t o) -> Member k a (Final t)
      fixMember name (Prop PropDef{..}) = Final (propRegister name outs) propIn outs
        where outs = propOutFun fixed
  (members, connectors :: [Qml.ObjRef () -> Moment t ()]) <- execWriterT $
    forQObject_ fixed $ \(Final register _ _) -> do
      (member, connect) <- lift register
      tell ([member], [connect])
  obj <- liftIO $ Qml.newClass members >>= flip Qml.newObject ()
  mapM_ ($ obj) connectors
  return $ QmlObject obj fixed

-- | A class for object types, which provides the required functionality for traversing
-- the member of the object.
--
-- To create an instance of this class, you can use GHC's generic deriving mechanism.
-- To do that, you first need to derive the 'Generic1' class (from module @GHC.Generics@, needs extension @-XDeriveGeneric@). Then, write an empty instance of 'QObject':
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
-- import GHC.Generics
-- ...
-- data Foo p = Foo { bar :: ... } deriving Generic1
-- instance QObject Foo
-- @
class QObject o where
  -- | Traverse each member of the object. The traversal function gets access to the
  -- name of the member (in derived instances, this is the record selector name) and
  -- the member itself.
  itraverseQObject :: Applicative f
                   => (forall k a. String -> Member k a p -> f (Member k a q)) -> o p -> f (o q)
  default itraverseQObject :: (Applicative f, Generic1 o, GQObject (Rep1 o)) =>
            (forall k a. String -> Member k a p -> f (Member k a q)) -> o p -> f (o q)
  itraverseQObject f = fmap to1 . gitraverseQObject f . from1
                          

-- | Like 'itraverseQObject', but only map a function without performing effects.
imapQObject :: QObject o => (forall k a. String -> Member k a p -> Member k a q) -> o p -> o q
imapQObject f = runIdentity . itraverseQObject (fmap Identity . f)

-- | Like 'itraverseQObject', but ignore the return value.
forQObject_ :: (QObject o, Applicative f) => o p -> (forall k a. Member k a p -> f ()) -> f ()
forQObject_ o f = void $ itraverseQObject (\_ a -> None <$ f a) o
--------------------------------------------------------------------------------

-- | Class for reporting errors during the generation of a QObject
class QObjectDerivingError t (k :: Symbol) | t -> k where
  -- | Allows to create any type. This means that there can never be an instance for
  -- this class, which is expected.
  err :: t -> a

-- | GHC.Generics deriving support
class GQObject o where
  -- | Generic itraverseQObject implementation.
  gitraverseQObject :: Applicative f
                    => (forall k a. String -> Member k a p -> f (Member k a q)) -> o p -> f (o q)
  default gitraverseQObject :: (QObjectDerivingError () x, Applicative f) =>
            (forall k a. String -> Member k a p -> f (Member k a q)) -> o p -> f (o q)
  gitraverseQObject _ _ = err ()

instance GQObject (K1 i c) where
  gitraverseQObject _ (K1 c) = pure (K1 c)

instance GQObject o => GQObject (M1 i c o) where
  gitraverseQObject f (M1 o) = M1 <$> gitraverseQObject f o

instance (GQObject a, GQObject b) => GQObject (a :*: b) where
  gitraverseQObject f (a :*: b)
    = (:*:) <$> gitraverseQObject f a <*> gitraverseQObject f b

instance (Traversable f, GQObject o) => GQObject (f :.: o) where
  gitraverseQObject f (Comp1 c) = Comp1 <$> traverse (gitraverseQObject f) c

instance Selector s => GQObject (M1 S s (Rec1 (Member k a))) where
  gitraverseQObject f s@(M1 (Rec1 m)) = M1 . Rec1 <$> f (selName s) m

instance QObjectDerivingError () "Object type must have exactly one constructor"
         => GQObject V1

instance QObjectDerivingError () "Object type must have at least one field"
         => GQObject U1

instance QObjectDerivingError () "Object type cannot have multiple constructors"
         => GQObject (a :+: b)

instance QObjectDerivingError () "Object type must have record selectors"
         => GQObject (M1 S NoSelector a)

instance QObjectDerivingError () "Object type tag can only appear in member fields"
         => GQObject (Rec1 f)

instance QObjectDerivingError () "Object type tag can only appear in member fields"
         => GQObject Par1
--------------------------------------------------------------------------------

-- | Object type tag for an object definition. @t@ is the reactive-banana time type,
-- while @f@ is the type of the object itself.
data DefWith t (f :: * -> *)

-- | Type of an object definition for the object type @f@ with the reactive-banana time
-- type @t@.
type Def t f = f (DefWith t f)

-- | Type for the final object with the reactive-banana time type @t@.
data Final t

-- | This is an internal type, used to create a dummy member that contains no data.
-- One case this is required is the implementation of 'forQObject_'.
data None

-- | Tag for a viewable property member. A viewable property can only be modified from
-- Haskell and not from QML. It can be read from QML.
data View

-- | Tag for a mutable property member. A mutable property can be modified and read
-- from both Haskell and QML.
--
-- Whenever the QML code changes the property, a signal is emitted.
-- The value of the property is still controlled by the Haskell code, so it has the
-- choice to accept the change signal and change the property or leave it as it is.
data Mut

-- | Tag for a static property member. A static property cannot change and stays
-- constant for the whole run of the program.
data Static

-- | Type of the outputs a property produces. These need to be specified by the user.
type family PropOutputs k t a
type instance PropOutputs View   t a = Behavior t a
type instance PropOutputs Mut    t a = Behavior t a
type instance PropOutputs Static t a = a

-- | Type of inputs a property can generate. These are supplied by the library.
type family PropInputs k t a
type instance PropInputs View   t a = ()
type instance PropInputs Mut    t a = Event t a
type instance PropInputs Static t a = ()

-- | Register as a member of an object. First element contains the member, the second
-- tuple element specifies an action to perform after the object has been created.
type Register t = Moment t (Qml.Member (), Qml.ObjRef () -> Moment t ())

-- | Generic definition of a property.
data PropDef f t k a = PropDef
  { propOutFun :: f (Final t) -> PropOutputs k t a
  , propIn  :: PropInputs k t a
  , propRegister :: String -> PropOutputs k t a -> Register t
  }

-- | A member of an object type.
--
-- The first type argument, @k@, specifies the kind of the member, currently one of
-- 'View', 'Mut' or 'Static'.
-- The second argument specifies the type (meaning differs based on the kind of the
-- member, but for properties, this is the type of the contained value).
-- The third type argument is the object type tag, which should be set to the type
-- argument given to the object type.
data Member k a p where
  Prop   :: PropDef f t k a -> Member k a (DefWith t f)
  Final  :: Register t -> PropInputs k t a -> PropOutputs k t a -> Member k a (Final t)
  None   :: Member k a None

-- | Create a new static property member. The argument is a function from the final
-- object to the value of the property. This allows the value to depend on other members.
static :: (Qml.Marshal a, Qml.CanReturnTo a ~ Qml.Yes)
       => (f (Final t) -> a) -> Member Static a (DefWith t f)
static f = Prop $ PropDef f () $ \name val ->
  pure (Qml.defPropertyConst' name (const $ pure val), const $ pure ())

-- | Event that is emitted whenever the given mutable property is changed from QML.
changed :: Member Mut a (Final t) -> Event t a
changed (Final _ e _) = e

-- | Class for members that can produce a behavior.
class HasBehavior k where
  -- | The behavior containing the current value of the given member.
  behavior :: Member k a (Final t) -> Behavior t a

-- | Static members always have the same value, so the behavior is constant
instance HasBehavior Static where behavior (Final _ _ v) = pure v
instance HasBehavior View where behavior (Final _ _ b) = b
instance HasBehavior Mut where behavior (Final _ _ b) = b

-- | Properties that can be constructed from behaviors. The behavior fully specifies
-- the value of the property at any moment in time.
-- Instances include 'Mut' and 'View' properties.
class AsProperty k a where
  -- | Construct a new property from a behavior, which may depend on other properties.
  prop :: Frameworks t
       => (f (Final t) -> Behavior t a) -> Moment t (Member k a (DefWith t f))

instance (Qml.Marshal a, Qml.CanReturnTo a ~ Qml.Yes) => AsProperty View a where
  prop f = pure $ Prop $ PropDef f () $ \name b -> do
    (sig, ref, connect) <- makeStore b
    return (Qml.defPropertySigRO' name sig (const $ readIORef ref), connect)

instance (Qml.Marshal a, Qml.CanReturnTo a ~ Qml.Yes, Qml.CanGetFrom a ~ Qml.Yes) => AsProperty Mut a where
  prop f = flip fmap newEvent $ \(event, handler) -> Prop $ PropDef f event $ \n b -> do
    (sig, ref, connect) <- makeStore b
    pure (Qml.defPropertySigRW' n sig (const $ readIORef ref) (const handler), connect)

-- | Make an IORef for the given behavior that always contains the current value of the
-- behavior. Returns a signal that is emited whenever the property is changed.
--
-- To emit the signal, a hook is returned that needs to be called once on the constructed
-- qml object reference to register the signal handler.
makeStore :: Frameworks t => Behavior t a
          -> Moment t (Qml.SignalKey (IO ()), IORef a, Qml.ObjRef () -> Moment t ())
makeStore b = do
  sig <- liftIO Qml.newSignalKey
  ref <- liftIO . newIORef =<< initial b
  let connect obj = do
        bChanged <- changes b
        reactimate' $ fmap (\v -> writeIORef ref v >> Qml.fireSignal sig obj) <$> bChanged
  return (sig, ref, connect)
