{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Typeable
import GHC.Generics
import Graphics.QML.Engine
import Graphics.QML.React
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

import qualified Data.Traversable as T

data StateView p = StateView
  { stateviewValue    :: Member Mut Int p
  , stateviewComputed :: Member View Int p
  } deriving (Generic1, Typeable)
instance QObject StateView where
  itraverseQObject = itraverseQObjectStripPrefix

stateView :: Frameworks t => Int -> StateView (Final t) -> Def t StateView
stateView x = \StateView{..} -> StateView
  (prop $ stepper x $ changed stateviewValue)
  (prop $ (2 *) <$> behavior stateviewValue)

data App p = App
  { page :: Member Mut Int p
  , currentState :: Member Embed [Object StateView Current] p
  , addEntry :: Member Fun (Int -> Int) p
  } deriving (Generic1, Typeable)
instance QObject App

config :: EngineConfig
config = defaultEngineConfig { initialDocument = fileDocument "./example/playground.qml" }

startEventLoop :: IO ()
startEventLoop = do
  l <- newEmptyMVar
  void . forkIO $ do
    catch (runEventLoop . liftIO $ putMVar l () >> forever (threadDelay maxBound)) $ \e ->
      const (putMVar l ()) $ (e :: EventLoopException)
  takeMVar l

initialStates :: [[Int]]
initialStates =
  [ [1,2,3,4,5]
  , [2,3,4,6,1]
  , [5,5,21,4]
  , [1,6,8,9,0,1,4]
  , [10]
  ]

main :: IO ()
main = do
  startEventLoop
  putStrLn "Started event loop"
  void $ requireEventLoop $ runQMLReact config $ namespace "app" $ object $ \App{..} ->
    let
      db = accumS initialStates $ unions
             [ update <$> behavior currentState <*> behavior page <@ changed page
             , create <$> behavior page <@> result addEntry
             ]
      
      update st 0 (_:sts) = map (value . stateviewValue . objectValue) st : sts
      update st n (s:sts) = s : update st (n - 1) sts
      update _ _ x = x

      create 0 entry (x:xs) = (x ++ [entry]) : xs
      create p entry (x:xs) = x : create (p - 1) entry xs
      create _ _ x = x

      viewState :: [[Int]] -> Int -> Embedded [Object StateView Current]
      viewState sts t
        = T.sequenceA [embedObject (Just i) $ stateView x | (i,x) <- zip [1..] $ sts !! t]
    in App
      { page = prop $ stepper 1 $ max 0 . min 4 <$> changed page
      , currentState = embed $ viewState <$> db <*> tracking page
      , addEntry = prop $ pure id
      }
