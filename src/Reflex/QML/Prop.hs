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
module Reflex.QML.Prop
  ( constant
  , readonly
  , mutable
  , mutableHold
  , namespace
  , method
  , methodVoid
  ) where

import Control.Monad.Writer
import Data.IORef
import Graphics.QML.Marshal
import Graphics.QML.Objects
import Reflex hiding (constant)
import Reflex.QML.Internal.AppHost
import Reflex.QML.Internal.Object

constant :: (Marshal a, CanReturnTo a ~ Yes, Monad m) => String -> Object m a -> ObjectT m ()
constant name oval = do
  val <- lift $ registerObjectEvents oval
  tellDefM . pure . objMember $ defPropertyConst' name (\_ -> return val)

readonly :: (Marshal a, CanReturnTo a ~ Yes, MonadAppHost t m)
         => String -> Dynamic t (Object m a) -> ObjectT m ()
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

mutable :: (Marshal a, CanReturnTo a ~ Yes, CanGetFrom a ~ Yes, MonadAppHost t m)
        => String -> Dynamic t (Object m a) -> ObjectT m (Event t a)
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

mutableHold :: (Marshal a, CanReturnTo a ~ Yes, CanGetFrom a ~ Yes, MonadAppHost t m)
            => String -> a -> ObjectT m (Dynamic t a)
mutableHold name initial = mdo
  valD <- holdDyn initial $ changed
  changed <- mutable name =<< mapDyn pure valD
  pure valD

namespace :: MonadIO m => String -> ObjectT m a -> ObjectT m a
namespace name obj = do
  (ref, a) <- lift $ runObjectT obj
  constant name ref
  return a

data AnyMethodSuffix a = forall ms. MethodSuffix ms => AnyMethodSuffix (IO a -> ms)

class MethodSignature a where
  type MethodResultFst a :: *
  type MethodResultSnd a :: *
  methodInternal :: ((MethodResultFst a, MethodResultSnd a) -> IO ()) -> AnyMethodSuffix a

instance (MethodSignature b, Marshal a, CanGetFrom a ~ Yes) => MethodSignature (a -> b) where
  type MethodResultFst (a -> b) = MethodResultFst b
  type MethodResultSnd (a -> b) = MethodResultSnd b
  methodInternal callback = case methodInternal callback of
    AnyMethodSuffix ms -> AnyMethodSuffix $ \f x -> ms $ fmap ($ x) f

instance (CanReturnTo b ~ Yes, Marshal b) => MethodSignature (a,b) where
  type MethodResultFst (a,b) = a
  type MethodResultSnd (a,b) = b
  methodInternal callback = AnyMethodSuffix $ \v -> v >>= \r -> snd r <$ callback r

method :: (MethodSignature s, MonadIO m, MonadAppHost t m)
       => String -> Dynamic t s -> ObjectT m (Event t (MethodResultFst s, MethodResultSnd s))
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

methodVoid :: (MonadIO m, MonadAppHost t m, MethodSignature s)
           => String -> Dynamic t s -> ObjectT m (Event t (MethodResultFst s))
methodVoid name funD = fmap fst <$> method name funD
