{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
module Reflex.QML.Internal.Run where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Typeable
import Graphics.QML
import Reflex.QML.Internal.AppHost
import Reflex.QML.Internal.Object
import Reflex.Spider

data ApplicationExited = ApplicationExited deriving (Show, Typeable)
instance Exception ApplicationExited

handleExitException :: ApplicationExited -> IO ()
handleExitException ApplicationExited = return ()

hostQmlApp :: EngineConfig -> (forall t m. MonadAppHost t m => ObjectT m ()) -> RunQML ()
hostQmlApp config app = liftIO . handle handleExitException $ do
  -- This variable will hold the context object as soon as the reflex thread
  -- has initialized itself
  objVar <- newEmptyMVar

  -- Remember the thread id of the main thread, to kill it when we want to exit
  mainThreadId <- myThreadId

  -- The reflex thread. Since running the QML engine blocks the main thread
  -- (we can't run use runEngineAsync because then we don't know when to stop),
  -- a thread is used to process the reflex events.
  let reflexThread = do
        runSpiderHost $ hostApp $ execObjectT app >>= liftIO . putMVar objVar
        throwTo mainThreadId ApplicationExited

  -- Now start the reflex thread and run the QML engine in the main thread after the
  -- reflex thread has initialized itself far enough to produce the context object.
  withAsync reflexThread . const $ do
    obj <- takeMVar objVar
    void . requireEventLoop $ runEngine config
      { contextObject = Just obj
      }
