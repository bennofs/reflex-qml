{-# LANGUAGE RankNTypes #-}
-- | Main module of the library. Import this to use the library
--
-- You probably also want to import 'Reflex.QML.Prop' to create properties.
module Reflex.QML
  ( hostQmlApp
  , mainQmlApp
  , ObjectBuilder()
  , runObjectBuilder
  , buildObject
  , Object()
  ) where

import Control.Concurrent
import Control.Exception (catch)
import Control.Monad
import Control.Monad.Trans
import Graphics.QML.Engine
import Reflex.Host.App
import Reflex.QML.Internal

-- | Starts the QML event loop. This works better when using GHCi, since it
-- won't fail if the event loop is already running.
startEventLoop :: IO ()
startEventLoop = do
  l <- newEmptyMVar
  void . forkIO $
    catch (runEventLoop . liftIO $ putMVar l () >> forever (threadDelay maxBound)) $ \e ->
      (e :: EventLoopException) `seq` putMVar l ()
  takeMVar l

-- | Run a QML application by loading the given QML document.
--
-- This is the simplest entry point to run a QML application.
-- If you need more control over the engine, you can also use `hostQmlApp`.
--
-- The first argument is an IO action, which makes it convenient to use
-- together with cabal's @Paths_@ module.
--
-- Example usage:
-- @
-- main :: IO ()
-- main = mainQmlApp (getDataFileName "qml/Application.qml") $ do
--   Prop.constantP "message" ("Hello world" :: Text)
-- @
mainQmlApp
  :: IO FilePath -- ^ The action to get the main QML document file path
  -> (forall t m. MonadAppHost t m => ObjectBuilder m ()) -- ^ The QML app
  -> IO ()
mainQmlApp getDocumentPath app = do
  document <- getDocumentPath
  startEventLoop
  let
    engineConfig = defaultEngineConfig
      { initialDocument = fileDocument document
      }
  requireEventLoop $ hostQmlApp engineConfig app
