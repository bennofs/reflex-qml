{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Reactive-banana based interface to HsQML
module Graphics.QML.React
  ( -- * Running
    runQMLReactLoop
  , runQMLReact

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
  , Static()
  , static
  , Var()
  , View()
  , Mut()
  , changed
  , AsProperty(..)
  , HasBehavior(..)
  , Fun()
  , result
  , MethodResult
  , MethodSignature
  , Def(), MemberDef(), DefWith(), Final()
  ) where

import GHC.Exts
import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad.Identity
import Control.Monad.Writer hiding (Product)
import Data.IORef
import Data.Maybe
import Data.Traversable (Traversable(traverse))
import GHC.Generics
import GHC.TypeLits
import Reactive.Banana hiding (Identity)
import Reactive.Banana.Frameworks
import Prelude -- avoid FTP related warnings

import qualified Graphics.QML as Qml
--------------------------------------------------------------------------------

-- | Run a reactive QML application.
--
-- This function compiles the supplied reactive-banana network, actuates it and
-- runs the QML engine with the specified engine config. It also starts the Qt event loop.
-- This function will not return until the engine has terminated.
--
-- warning: the context object configuration option is overriden by this function
runQMLReactLoop :: Qml.EngineConfig -> (forall t. Frameworks t => Moment t (QmlObject t o)) -> IO ()
runQMLReactLoop config n = Qml.runEventLoop $ runQMLReact config n

-- | Run a reactive QML application, without starting the event loop.
--
-- This function is like 'runQMLReactLoop', but doesn't start the event loop. This is
-- useful if you want to control the Qt event loop yourself (for example, when running
-- in GHCi).
--
-- This function also blocks until the engine has terminated.
runQMLReact :: Qml.EngineConfig -> (forall t. Frameworks t => Moment t (QmlObject t o)) -> Qml.RunQML ()
runQMLReact config networkDefinition = do
  (end, obj) <- liftIO $ do
    objVar <- newEmptyMVar
    network <- compile (networkDefinition >>= liftIO . putMVar objVar . objectRef)
    actuate network
    (,) (pause network) <$> takeMVar objVar
  Qml.runEngine config { Qml.contextObject = Just obj }
  liftIO end

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
objectRef = Qml.anyObjRef . rawObjectRef

-- | Return the raw reference to the object, exposing the internal representation.
--
-- This function should not be exported.
rawObjectRef :: QmlObject t o -> Qml.ObjRef ()
rawObjectRef (QmlObject r _) = r

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
  let fixed = fixObject o
  (members, connectors :: [Qml.ObjRef () -> Moment t ()]) <- execWriterT $
    forQObject_ fixed $ \(Final register _ _) -> do
      (member, connect) <- lift register
      tell (maybeToList member, [connect])
  obj <- liftIO $ Qml.newClass members >>= flip Qml.newObject ()
  mapM_ ($ obj) connectors
  return $ QmlObject obj fixed

-- | Fix a object definition, passing the result back in so that members can depend
-- upon each other.
fixObject :: QObject o => Def t o -> o (Final t)
fixObject o = fix $ \r -> imapQObject (fixMember r) o where
  fixMember :: o (Final t) -> String -> Member k a (DefWith t o) -> Member k a (Final t)
  fixMember r name (Def PropDef{..}) = Final (propRegister name outs) propIn outs where
    outs = propOutFun r

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
-- data Foo p = Foo { bar :: T, ... } deriving Generic1
-- instance QObject Foo
-- @
--
-- When there is an error while deriving the class, the error messages will look like
-- this:
--
-- @
-- Could not deduce (QObjectDerivingError …
--                        () "Object type tag can only appear in member fields")
--      arising from a use of ‘Graphics.QML.React.$gdmitraverseQObject’
-- ...
-- @
--
-- In this case, look for the the @QObjectDerivingError@. It will show you an error
-- message explaining the problem.
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

-- | Class for reporting errors during the generation of a QObject.
class (ErrMsg t ~ k) => QObjectDerivingError t (k :: Symbol) where
  type ErrMsg t :: Symbol
  
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

instance (GQObject a, GQObject b) => GQObject (a :*: b) where
  gitraverseQObject f (a :*: b)
    = (:*:) <$> gitraverseQObject f a <*> gitraverseQObject f b

instance (Traversable f, GQObject o) => GQObject (f :.: o) where
  gitraverseQObject f (Comp1 c) = Comp1 <$> traverse (gitraverseQObject f) c


-- | Returns true if this M1 type represents a field selector that is a Member
type family IsMember i f where
  IsMember S (Rec1 (Member k a)) = 'True
  IsMember x y = 'False

-- | Checks that the selector type is not 'NoSelector' (this happens for types without
-- record selectors).
type family ValidSelector s :: Constraint where
  ValidSelector NoSelector
    = QObjectDerivingError () "Object type must have record selectors"
  ValidSelector a = ()

-- | Type class for dispatch based on IsMember
class (b ~ IsMember i g) => M1GQObject b i s g where
  m1gitraverseQObject :: Applicative f
                      => (forall k a. String -> Member k a p -> f (Member k a q))
                      -> M1 i s g p -> f (M1 i s g q)

instance (ValidSelector s, Selector s) => M1GQObject 'True S s (Rec1 (Member k a)) where
  m1gitraverseQObject f s@(M1 (Rec1 m)) = M1 . Rec1 <$> f (selName s) m

instance (QObject g, IsMember i g ~ 'False) => M1GQObject 'False i s g where
  m1gitraverseQObject f (M1 x) = M1 <$> itraverseQObject f x

instance M1GQObject (IsMember i f) i s f => GQObject (M1 i s f) where
  gitraverseQObject = m1gitraverseQObject

instance QObjectDerivingError () "Object type must have exactly one constructor"
         => GQObject V1

instance QObjectDerivingError () "Object type must have at least one field"
         => GQObject U1

instance QObjectDerivingError () "Object type cannot have multiple constructors"
         => GQObject (a :+: b)

instance QObjectDerivingError () "Object type tag can only appear in member fields"
         => GQObject (Rec1 f)

instance QObjectDerivingError () "Object type tag can only appear in member fields"
         => GQObject Par1
--------------------------------------------------------------------------------

-- | Object type tag for an object definition. @t@ is the reactive-banana time type,
-- while @f@ is the type of the object itself.
data DefWith t (f :: * -> *)

-- | Type of an object definition for the object type @o@ with the reactive-banana time
-- type @t@.
--
-- Each member field of the definition has type @MemberDef t o k a@.
type Def t o = o (DefWith t o)

-- | Definition of a member of the object type @o@ with kind @k@, type @a@ and @t@ as
-- the reactive-banana time.
type MemberDef t o k a = Member k a (DefWith t o)

-- | Object type tag for the final object with the reactive-banana time type @t@.
--
-- The final object contains the values, behaviors and events for all members.
data Final t

-- | This is an internal type, used to create a dummy member that contains no data.
-- One case this is required is the implementation of 'forQObject_'.
data None

-- | A member of an object type.
--
-- The type arguments specify the kind of the member (@k@, one of 'Static', 'Var',
-- 'View', 'Mut' or 'Fun'), the type of the member (@a@, meaning depends on member kind)
-- and the object type tag @p@ which should be set to the object type tag passed to the
-- owner of this property.
data Member k a p where
  Def    :: PropDef f t k a -> Member k a (DefWith t f)
  Final  :: Register t -> MemberInputs k t a -> MemberOutputs k t a -> Member k a (Final t)
  None   :: Member k a None

-- | Type of the outputs a member produces. These need to be specified by the user.
type family MemberOutputs k t a

-- | Type of inputs a member can generate. These are supplied by the library.
type family MemberInputs k t a

-- | Register as a member of an object.
--
-- First element contains the member (may be 'Nothing', which means there is no QML
-- member for this property definition), the second tuple element
-- is the action to perform after the object has been created.
type Register t = Moment t (Maybe (Qml.Member ()), Qml.ObjRef () -> Moment t ())

-- | Generic definition of a property.
data PropDef o t k a = PropDef
  { propOutFun :: o (Final t) -> MemberOutputs k t a
  , propIn  :: MemberInputs k t a
  , propRegister :: String -> MemberOutputs k t a -> Register t
  }

-- | Class for members that can produce a behavior.
class HasBehavior k where
  -- | The behavior containing the current value of the given member.
  behavior :: Member k a (Final t) -> Behavior t a

-- | Properties that can be constructed from behaviors.
--
-- The behavior fully specifies the value of the property at any moment in time.
-- Instances include the 'Mut', 'Fun' and 'View' properties.
class AsProperty k a where
  -- | Construct a new property from a behavior, which may depend on other properties.
  prop :: Frameworks t
       => (o (Final t) -> Behavior t a) -> Moment t (MemberDef t o k a)

-- | Make an IORef for the given behavior that always contains the current value of the
-- behavior. Returns a signal that is emited whenever the property is changed.
--
-- To emit the signal, a hook is returned that needs to be called once on the constructed
-- qml object reference to register the signal handler.
--
-- This is often used to implement AsProperty instances.
makeStore :: Frameworks t => Behavior t a
          -> Moment t (Qml.SignalKey (IO ()), IORef a, Qml.ObjRef () -> Moment t ())
makeStore b = do
  sig <- liftIO Qml.newSignalKey
  ref <- liftIO . newIORef =<< initial b
  let connect obj = do
        bChanged <- changes b
        reactimate' $ fmap (\v -> writeIORef ref v >> Qml.fireSignal sig obj) <$> bChanged
  return (sig, ref, connect)

--------------------------------------------------------------------------------

-- | Kind of a static property member.
--
-- A static property cannot change and stays constant for the whole run of the program.
-- It is readable from QML, but cannot be modified from either QML or Haskell.
data Static
type instance MemberOutputs Static t a = a
type instance MemberInputs Static t a = ()

-- | Static members always have the same value, so the behavior is constant.
instance HasBehavior Static where behavior (Final _ _ v) = pure v

-- | Create a new static property member.
--
-- The argument is a function from the final object to the value of the property.
-- This allows the value to depend on other members.
static :: (Qml.Marshal a, Qml.CanReturnTo a ~ Qml.Yes)
       => (o (Final t) -> a) -> Moment t (MemberDef t o Static a)
static f = pure $ Def $ PropDef f () $ \name val ->
  pure (Just . Qml.defPropertyConst' name . const $ pure val, const $ pure ())

-- | Kind of a member that exists only on the haskell side and is not exposed to QML.
--
-- It can be used for state that the haskell code needs to keep that cannot be exposed
-- to QML because it is not marshallable or because it should remain private.
data Var
type instance MemberOutputs Var t a = Behavior t a
type instance MemberInputs  Var t a = ()
instance HasBehavior Var where behavior (Final _ _ b) = b

instance AsProperty Var a where
  prop f = pure $ Def $ PropDef f () $ \_ _ -> pure (Nothing, const $ pure ())

-- | Kind of a viewable property member.
--
-- A viewable property can only be modified from Haskell and not from QML.
-- It can be read from QML.
data View
type instance MemberOutputs View t a = Behavior t a
type instance MemberInputs View  t a = ()
instance HasBehavior View where behavior (Final _ _ b) = b

-- | For this instance, it must be possible to return the property type to QML. This is
-- why it requires the property type to have 'Qml.Marshal' instance.
instance (Qml.Marshal a, Qml.CanReturnTo a ~ Qml.Yes) => AsProperty View a where
  prop f = pure $ Def $ PropDef f () $ \name b -> do
    (sig, ref, connect) <- makeStore b
    return (Just $ Qml.defPropertySigRO' name sig (const $ readIORef ref), connect)

-- | Kind of a mutable property member. A mutable property can be modified and read
-- from both Haskell and QML.
--
-- Whenever the QML code changes the property, a signal is fired.
-- The value of the property is still controlled by the Haskell code, so it has the
-- choice to accept the change signal and change the property or leave it as it is.
data Mut
type instance MemberInputs Mut t a = Event t a
type instance MemberOutputs Mut t a = Behavior t a
instance HasBehavior Mut where behavior (Final _ _ b) = b

-- | Event that is fired whenever the given mutable property is changed from QML.
--
-- The value of the event is equal to the value the property was set to from QML.
changed :: Member Mut a (Final t) -> Event t a
changed (Final _ e _) = e

-- | For this instance, it must be possible to return the property to QML and read it get
-- QML. This is why it requires the property type to have 'Qml.Marshal' instance.
instance (Qml.Marshal a, Qml.CanReturnTo a ~ Qml.Yes, Qml.CanGetFrom a ~ Qml.Yes) => AsProperty Mut a where
  prop f = do
    (event, handler) <- newEvent
    pure . Def $ PropDef f event $ \n b -> do
      (sig, ref, connect) <- makeStore b
      let member = Qml.defPropertySigRW' n sig (const $ readIORef ref) (const handler)
      pure (Just member, connect)

-- | Kind of a member method. Members with this kind can be called as functions from QML.
--
-- The property type for this kind should equal the function signature of the member.
-- So a @Member Fun (Int -> Int) p@ specifies a method that takes an Int
-- and returns an Int. This method cannot do any IO.
--
-- To do IO, you need to use @Member Fun (Int -> IO Int)@, which also allows the
-- method to perform some IO to produce it's result.
data Fun
type instance MemberInputs Fun t a = Event t (MethodResult a)
type instance MemberOutputs Fun t a = Behavior t a

-- | Computes the result type of a function with parameters. If the result type is an IO
-- action, the type returned by the IO action is returned.
--
-- Examples:
--
-- @
-- MethodResult (() -> Int) = Int
-- MethodResult (IO Int) = Int
-- MethodResult (a -> b -> c) = c
-- MethodResult (a -> b -> IO c) = c
-- MethodResult (a -> IO (b -> c)) = b -> c
-- @
type family MethodResult a where
  MethodResult (a -> b) = MethodResult b
  MethodResult (IO b) = b
  MethodResult b = b

-- | Return the result of the last call of this member method.
--
-- The returned event fires whenever the function is called from QML.
result :: Member Fun a (Final t) -> Event t (MethodResult a)
result (Final _ i _) = i

instance MethodSignature a => AsProperty Fun a where
  prop f = do
    (resultEvent, resultHandler) <- newEvent
    pure . Def $ PropDef f resultEvent $ \name fun -> do
      (inputsEvent, inputsHandler) <- newEvent
      resultsVar <- liftIO newEmptyMVar
      let results = applyToArgs <$> fun <@> inputsEvent
      reactimate $ (>>= resultHandler) <$> results
      reactimate $ putMVar resultsVar <$> resultEvent
      case toMethodSuffix :: ToMethodSuffix a (IO (MethodResult a)) of
        ToMethodSuffix mk ->
          let handler x = inputsHandler x >> takeMVar resultsVar
          in pure (Just $ Qml.defMethod' name (const $ mk handler), const $ pure ())

data ToMethodSuffix def ms
  = forall ms'. Qml.MethodSuffix ms' => ToMethodSuffix ((MethodArgs def -> ms) -> ms')

-- | Return a type that can hold the arguments for the given function type.
--
-- Returns a chain of tuples, ending with either @IO ()@ or @()@, depending on
-- the return type of the function.
--
-- Examples:
--
-- @
-- MethodArgs (IO a) = IO ()
-- MethodArgs ()     = ()
-- MethodArgs (a -> IO b) = (a, IO ())
-- MethodArgs (a -> b -> c -> Int) = (a, (b, (c, ())))
-- @
type family MethodArgs a where
  MethodArgs (a -> b) = (a, MethodArgs b)
  MethodArgs (IO a)  = IO ()
  MethodArgs a       = ()

-- | Constraint for valid method signatures.
--
-- For valid method signatures, each method argument must be an instance
-- of 'Qml.Marshal' and gettable from QML. The result type must also be an
-- instance of 'Qml.Marshal' and be returnable to QML.
type MethodSignature def = MethodSignatureImpl (MethodArgs def) def

-- | This class is the implementation of the @MethodSignature@ class.
-- It has an extra type argument for the @MethodArgs@ type to avoid OverlappingInstances.
class (MethodArgs def ~ x, Qml.Marshal (MethodResult def),
       Qml.CanReturnTo (MethodResult def) ~ Qml.Yes) => MethodSignatureImpl x def where
  toMethodSuffix :: Qml.MethodSuffix ms => ToMethodSuffix def ms
  applyToArgs :: def -> MethodArgs def -> IO (MethodResult def)

instance (Qml.Marshal a, Qml.CanReturnTo a ~ Qml.Yes)
 => MethodSignatureImpl (IO ()) (IO a) where
  toMethodSuffix = ToMethodSuffix ($ return ())
  applyToArgs = const

instance (Qml.Marshal a, Qml.CanReturnTo a ~ Qml.Yes, MethodArgs a ~ (),
          a ~ MethodResult a) => MethodSignatureImpl () a where
  toMethodSuffix = ToMethodSuffix ($ ())
  applyToArgs = const . return

instance (Qml.Marshal a, Qml.CanGetFrom a ~ Qml.Yes, MethodSignature b, y ~ MethodArgs b)
 => MethodSignatureImpl (a,y) (a -> b) where
  toMethodSuffix = case fixType toMethodSuffix of
    ToMethodSuffix mk -> ToMethodSuffix $ \f x -> mk (f . (,) x)
   where
    fixType :: ToMethodSuffix b ms -> ToMethodSuffix b ms
    fixType = id
  applyToArgs f = applyToArgs <$> f . fst <*> snd
