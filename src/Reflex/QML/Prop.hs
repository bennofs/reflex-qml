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
  ) where

import Control.Concurrent.MVar
import Control.Monad.Writer
import Data.IORef
import Data.Proxy
import Graphics.QML.Marshal
import Graphics.QML.Objects
import Reflex hiding (constant)
import Reflex.Host.Class
import Reflex.QML.Internal

constant :: (Marshal a, CanReturnTo a ~ Yes, Monad m)
         => String -> a -> QApp t m ()
constant name val = QApp . tell $ ObjectSpec
  [defPropertyConst' name (\_ -> return val)]
  (const [])

readonly :: (Reflex t, MonadIO m, Marshal a, CanReturnTo a ~ Yes, MonadSample t m)
         => String -> Dynamic t a -> QApp t m ()
readonly name valD = do
  currentRef <- liftIO . newIORef =<< lift (sample $ current valD)
  sig <- liftIO newSignalKey
  QApp . tell $ ObjectSpec
    [defPropertySigRO' name sig (\_ -> readIORef currentRef)]
    (\obj -> [ffor (updated valD) $ \val -> liftIO $ do
      writeIORef currentRef val
      fireSignal sig obj
    ])

mutable :: (Reflex t, MonadIO m, Marshal a, CanReturnTo a ~ Yes, CanGetFrom a ~ Yes
          ,MonadSample t m, MonadReflexCreateTrigger t m)
        => String -> Dynamic t a -> QApp t m (Event t a)
mutable name valD = do
  (changedE, fireChanged) <- newEventWithFire
  currentRef <- liftIO . newIORef =<< lift (sample $ current valD)
  sig <- liftIO newSignalKey
  QApp . tell $ ObjectSpec
    [defPropertySigRW' name sig (\_ -> readIORef currentRef) (const $ void . fireChanged)]
    (\obj -> [ffor (updated valD) $ \val -> liftIO $ do
      writeIORef currentRef val
      fireSignal sig obj
    ])
  pure changedE

mutableHold :: (Reflex t, MonadIO m, Marshal a, CanReturnTo a ~ Yes, CanGetFrom a ~ Yes
              ,MonadSample t m, MonadHold t m, MonadReflexCreateTrigger t m, MonadFix m)
            => String -> a -> QApp t m (Dynamic t a)
mutableHold name initial = mdo
  valD <- holdDyn initial changed
  changed <- mutable name valD
  pure valD

namespace :: (MonadIO m, MonadHold t m, Reflex t, MonadReflexCreateTrigger t m)
          => String -> QApp t m a -> QApp t m a
namespace name app = do
  (objDyn, rDyn) <- subApp $ constDyn app
  readonly name objDyn
  lift . sample . current $ rDyn

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

method :: (MethodSignature s, MonadIO m, MonadReflexCreateTrigger t m, MonadSample t m)
       => String -> Dynamic t s -> QApp t m (Event t (MethodResult s))
method name funD = do
  (event, fire) <- newEventWithFire
  currentFunRef <- liftIO . newIORef =<< sample (current funD)
  let member = case methodInternal (void . fire) of
        AnyMethodSuffix ms -> defMethod' name $ \_ -> ms (readIORef currentFunRef)
  QApp . tell $ ObjectSpec
    [member]
    (const [ffor (updated funD) $ liftIO . writeIORef currentFunRef])
  return event
