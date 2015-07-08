{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Graphics.QML.Engine
import Reflex
import Reflex.Dynamic
import Reflex.QML.App

import qualified Reflex.QML.Prop as Prop
import qualified Data.Text as T

data TodoItem = TodoItem
  { description :: T.Text
  , completed :: Bool
  , editing :: Bool
  }

engineConfig :: EngineConfig
engineConfig = defaultEngineConfig
  { initialDocument = fileDocument "examples/todomvc/Main.qml"
  }

startEventLoop :: IO ()
startEventLoop = do
  l <- newEmptyMVar
  void . forkIO $
    catch (runEventLoop . liftIO $ putMVar l () >> forever (threadDelay maxBound)) $ \e ->
      (e :: EventLoopException) `seq` putMVar l ()
  takeMVar l

main :: IO ()
main = do
  startEventLoop
  requireEventLoop $ runApp engineConfig $ Prop.namespace "app" $ mdo
    newItem <- Prop.method "newItem" . constDyn $ \x -> (x :: T.Text,())
    todosSubapp <- foldDyn (:) [] $ ffor newItem $ \x -> mdo
      completedE <- Prop.mutable "completed" =<< holdDyn False completedE
      descriptionE <- Prop.mutable "description" =<< holdDyn False descriptionE
      editingE <- Prop.mutable "editing" =<< holdDyn False editingE
      return ()
    let f x = void $ traverse subApp x
    todosApp <- performDynamic =<< mapDyn f todosSubapp
    return ()
