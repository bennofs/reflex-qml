{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Graphics.QML.React where

import Data.Void
import Data.Constraint
import Control.Monad.Writer hiding (Product)
import Data.Functor.Product
import Data.Either
import Data.List (sortBy)
import Data.Ord
import Data.These
import Control.Monad.Identity
import Data.IORef
import Control.Applicative
import GHC.Exts
import GHC.Generics
import Graphics.QML
import Reactive.Banana hiding (Identity)
import Reactive.Banana.Frameworks

type Immutable = Behavior
data Mutable t a = Mutable
  { propValue :: Behavior t a
  , propChange :: Event t a
  }

class Diff object where
  ibitraverseObject :: Applicative f
                 => (forall a. String -> r a -> f (r' a))
                 -> (forall a. String -> w a -> f (w' a))
                 -> object r w -> f (object r' w')
  
  ibitraverseObjectDiff :: Applicative f
                     => (forall a. String -> These (r a) (r' a) -> f (r'' a))
                     -> (forall a. String -> These (w a) (w' a) -> f (w'' a))
                     -> object r w -> object r' w' -> f (object r'' w'')

bimapObject :: Diff o => (forall a. r a -> r' a) -> (forall a. w a -> w' a) -> o r w -> o r' w'
bimapObject fr fw
  = runIdentity . ibitraverseObject (const $ Identity . fr) (const $ Identity . fw)

newtype Actions m a = Actions { runActions :: m () } deriving Functor
instance Applicative f => Applicative (Actions f) where
  pure _ = Actions $ pure ()
  Actions a <*> Actions b = Actions (a *> b)

ibitraverseObject_ :: (Diff o, Applicative f)
                   => (forall a. String -> r a -> f ()) -> (forall a. String -> w a -> f ()) -> o r w -> f ()
ibitraverseObject_ fr fw = runActions . ibitraverseObject (wrap fr) (wrap fw) where
  wrap f x y = Actions (f x y)

type family QMLConstraint a v :: Constraint where
  QMLConstraint Mutable   a  = (Marshal a, CanReturnTo a ~ Yes, CanGetFrom a ~ Yes)
  QMLConstraint Immutable a  = (Marshal a, CanReturnTo a ~ Yes)

data Object (f :: (* -> *) -> (* -> *) -> *)
data Prim a

data MemberView a v where
  MutableMember :: View Mutable v -> Handler (QMLType v) -> MemberView Mutable v
  ImmutableMember :: View Immutable v -> MemberView Immutable v

data View a v where
  ObjectView :: Diff f => f (MemberView Immutable) (MemberView Mutable) -> View Immutable (Object f)
  ListView :: QMLConstraint Immutable (QMLType v) => [(Maybe Int, View Immutable v)] -> View Immutable [v]
  PrimitiveView :: (QMLConstraint a v, Eq v) => v -> View a (Prim v)

data MemberModel v = MemberModel (SignalKey (IO ())) (IORef (Model v))
data Model v where
  ObjectModel    :: Diff f => ObjRef () -> f MemberModel MemberModel -> Model (Object f)
  ListModel      :: [(Maybe Int, Model v)] -> Model [v]
  PrimitiveModel :: v -> Model (Prim v)

updateModelRef :: View a v -> IORef (Model v) -> IO () -> IO ()
updateModelRef view modelRef signalChange = do
  model <- readIORef modelRef
  (changed, model') <- updateModel view model
  when changed $ writeIORef modelRef model' >> signalChange

updateModel :: View a v -> Model v -> IO (Bool, Model v)
updateModel (ObjectView object) m@(ObjectModel ref model)
  | Just d <- ibitraverseObjectDiff withModel withModel object model =
  (False, m) <$ ibitraverseObject_ doUpdate doUpdate d
 where
  withModel _ = fmap (uncurry Pair) . justThese

  doUpdate :: String -> Product (MemberView a) MemberModel v -> IO ()
  doUpdate _ (Pair (ImmutableMember memberView) (MemberModel signalChange modelRef)) =
    updateModelRef memberView modelRef (fireSignal signalChange ref)
updateModel (ListView items) m@(ListModel model) = do
  matchings' <- forM matchings $ \(key, itemView, maybeModel) -> case maybeModel of
    Just itemModel -> do
      (changed, itemModel') <- updateModel itemView itemModel
      return (changed, (key, itemModel'))
    Nothing -> (,) True . (,) key <$> createModel itemView
  return (any fst matchings', ListModel $ snd <$> matchings')
 where
  match ((key, a) : as) ((key', b) : bs)
    | key == key' = (key, a, Just b) : match as bs
    | otherwise  = (key, a, Nothing) : match as ((key', b) : bs)
  match as _ = [(key, a, Nothing) | (key, a) <- as]
  matchings = match (sortBy (comparing fst) items) model
updateModel (PrimitiveView v) m@(PrimitiveModel v') | v == v' = return (False, m)
updateModel v _ = (,) True <$> createModel v

type family QMLType v where
  QMLType (Object f) = ObjRef ()
  QMLType [a]        = [QMLType a]
  QMLType (Prim a)   = a
type QMLConstraintDict a v = Dict (QMLConstraint a (QMLType v))
type ModelBuilderM v = WriterT [Member ()] IO (MemberModel v)

class QMLViewDict a where
  qmlViewDict :: View a v -> QMLConstraintDict a v

instance QMLViewDict Immutable where
  qmlViewDict (ObjectView _) = Dict
  qmlViewDict (ListView _) = Dict
  qmlViewDict (PrimitiveView _) = Dict

instance QMLViewDict Mutable where
  qmlViewDict (PrimitiveView _) = Dict

createModel :: View a v -> IO (Model v)
createModel (PrimitiveView v) = return $ PrimitiveModel v
createModel (ListView items)
  = ListModel <$> mapM (\(key, v) -> (,) key <$> createModel v) items
createModel (ObjectView object) = do
  (model, members) <- runWriterT $ ibitraverseObject immutableMember mutableMember object
  clazz <- newClass members
  obj <- newObject clazz ()
  return $ ObjectModel obj model
 where
  getter model _ = getModelValue <$> readIORef model
   
  immutableMember :: String -> MemberView Immutable v -> ModelBuilderM v
  immutableMember name (ImmutableMember view) = do
    sig <- liftIO (newSignalKey :: IO (SignalKey (IO ())))
    model <- liftIO $ createModel view >>= newIORef
    tell $ case qmlViewDict view of Dict -> [defPropertySigRO' name sig $ getter model]
    return $ MemberModel sig model

  mutableMember :: String -> MemberView Mutable v -> ModelBuilderM v
  mutableMember name (MutableMember view handler) = do
    sig <- liftIO (newSignalKey :: IO (SignalKey (IO ())))
    model <- liftIO $ createModel view >>= newIORef
    tell $ case qmlViewDict view of
      Dict -> [ defPropertySigRW' name sig (getter model) $  \_ v -> handler v ]
    return $ MemberModel sig model

  getModelValue :: Model v -> QMLType v
  getModelValue (ObjectModel obj _) = obj
  getModelValue (ListModel ls) = map (getModelValue . snd) ls
  getModelValue (PrimitiveModel v) = v
