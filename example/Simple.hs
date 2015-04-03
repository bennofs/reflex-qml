{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import GHC.Generics
import Graphics.QML.Engine
import Graphics.QML.React
import Reactive.Banana.Combinators

import qualified Data.Traversable as T

data StateView p = StateView
  { stateviewValue    :: Member Mut Int p
  , stateviewComputed :: Member View Int p
  } deriving Generic1
instance QObject StateView where
  itraverseQObject = itraverseQObjectStripPrefix

stateView :: Frameworks t => Int -> Moment t (Object (Behavior t) StateView)
stateView x = object $ \ ~StateView{..} -> StateView
  (prop $ stepper x $ changed stateviewValue)
  (prop $ (2 *) <$> behavior stateviewValue)

data App p = App
  { page :: Member Mut Int p
  , currentState :: Member Embed [ValueObject StateView] p
  , addEntry :: Member Fun (Int -> Int) p
  } deriving Generic1
instance QObject App

config :: EngineConfig
config = defaultEngineConfig { initialDocument = fileDocument "/tmp/test.qml" }

startEventLoop :: IO ()
startEventLoop = void . forkIO $ do
  catch (runEventLoop . liftIO . forever $ threadDelay maxBound) $ \e ->
    const (return ()) $ (e :: EventLoopException)

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
  void $ requireEventLoop $ runQMLReact config $ namespace "app" $ object $ \a@App{..} ->
    let
      db = accumB initialStates $ unions
             [ update <$> behavior currentState <*> behavior page <@ changed page
             , create <$> behavior page <@> result addEntry
             ]
      
      update st 0 (_:sts) = map (value . stateviewValue . objectValue) st : sts
      update st n (s:sts) = s : update st (n - 1) sts
      update _ _ x = x
     
      create 0 entry (x:xs) = (x ++ [entry]) : xs
      create p entry (x:xs) = x : create (p - 1) entry xs

      viewState :: Int -> [[Int]] -> Embedded [ValueObject StateView]
      viewState t sts
        = T.sequenceA [embedObject $ stateView x | x <- sts !! t]

    in App
      { page = prop $ stepper 1 $ max 0 . min 4 <$> changed page
      , currentState = embed $ viewState <$> behavior page <*> db
      , addEntry = prop $ pure id
      }
