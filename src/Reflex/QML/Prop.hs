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
  , method
  ) where

import Control.Monad.Writer
import Data.IORef
import Graphics.QML.Marshal
import Graphics.QML.Objects
import Reflex hiding (constant)
import Reflex.QML.Internal.AppHost
import Reflex.QML.Internal.Object

constant :: (Marshal a, CanReturnTo a ~ Yes, Monad m) => String -> a -> ObjectT m ()
constant name val = tellDefM . pure . objMember $ defPropertyConst' name (\_ -> return val)

readonly :: (Marshal a, CanReturnTo a ~ Yes, MonadAppHost t m)
         => String -> Dynamic t a -> ObjectT m ()
readonly name valD = do
  sig <- liftIO newSignalKey
  tellDefM $ do
    currentRef <- liftIO . newIORef =<< sample (current valD)
    pure $ mconcat
      [ objMember $ defPropertySigRO' name sig (\_ -> readIORef currentRef)
      , objRegister $ \obj -> performEvent_ $ ffor (updated valD) $ \val -> liftIO $ do
          writeIORef currentRef val
          fireSignal sig obj
      ]

mutable :: (Marshal a, CanReturnTo a ~ Yes, CanGetFrom a ~ Yes, MonadAppHost t m)
        => String -> Dynamic t a -> ObjectT m (Event t a)
mutable name valD = do
  (event, fire) <- lift newExternalEvent
  sig <- liftIO newSignalKey
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
  valD <- holdDyn initial changed
  changed <- mutable name valD
  pure valD

data AnyMethodSuffix a = forall ms. MethodSuffix ms => AnyMethodSuffix (IO a -> ms)

class MethodSignature a where
  type MethodResult a :: *
  methodInternal :: (MethodResult a -> IO ()) -> AnyMethodSuffix a

instance (MethodSignature b, Marshal a, CanGetFrom a ~ Yes) => MethodSignature (a -> b) where
  type MethodResult (a -> b) = MethodResult b
  methodInternal callback = case methodInternal callback of
    AnyMethodSuffix ms -> AnyMethodSuffix $ \f x -> ms $ fmap ($ x) f

instance (CanReturnTo b ~ Yes, Marshal b) => MethodSignature (a,b) where
  type MethodResult (a,b) = (a,b)
  methodInternal callback = AnyMethodSuffix $ \v -> v >>= \r -> snd r <$ callback r

method :: (MethodSignature s, MonadIO m, MonadAppHost t m)
       => String -> Dynamic t s -> ObjectT m (Event t (MethodResult s))
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
