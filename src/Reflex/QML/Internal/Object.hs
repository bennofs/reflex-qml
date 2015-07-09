{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Reflex.QML.Internal.Object where

import Control.Applicative
import Control.Monad.Writer
import Data.Semigroup.Applicative
import Graphics.QML.Objects
import Prelude
import Reflex.Class
import Reflex.Host.Class

import qualified Data.DList as DL

data ObjectDef m = ObjectDef
  { register :: ObjRef () -> m ()
  , members  :: DL.DList (Member ())
  }

objMember :: Applicative m => Member () -> ObjectDef m
objMember m = mempty { members = pure m }

objRegister :: (ObjRef () -> m ()) -> ObjectDef m
objRegister r = ObjectDef r mempty

instance Applicative m => Monoid (ObjectDef m) where
  mempty = ObjectDef (const $ pure ()) mempty
  mappend (ObjectDef r m) (ObjectDef r' m') = ObjectDef (liftA2 (*>) r r') (mappend m m')

newtype ObjectT m a = ObjectT { unObjectT :: WriterT (Ap m (ObjectDef m)) m a }
  deriving (Functor, Applicative, Monad, MonadHold t, MonadSample t,
            MonadReflexCreateTrigger t, MonadFix, MonadIO)

instance MonadTrans ObjectT where
  lift = ObjectT . lift

--------------------------------------------------------------------------------
tellDefM :: Monad m => m (ObjectDef m) -> ObjectT m ()
tellDefM = ObjectT . tell . Ap

runObjectT :: MonadIO m => ObjectT m a -> m (AnyObjRef, a)
runObjectT (ObjectT m) = do
  (a, ObjectDef{..}) <- traverse getApp =<< runWriterT m
  obj <- liftIO $ newClass (DL.toList members) >>= flip newObject ()
  register obj
  return (anyObjRef obj, a)

execObjectT :: MonadIO m => ObjectT m () -> m AnyObjRef
execObjectT = fmap fst . runObjectT
