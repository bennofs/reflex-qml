{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Concurrent
import Control.Monad
import GHC.Generics
import Graphics.QML.Engine
import Graphics.QML.React
import Reactive.Banana
import Reactive.Banana.Frameworks

data App t = App
  { time :: Member View Int t
  , increment :: Member Mut Int t
  } deriving Generic1
instance QObject App

callEvery :: Int -> AddHandler ()
callEvery delay = AddHandler $ \handler -> fmap killThread $ forkIO $
  forever (handler () >> threadDelay delay)

config :: EngineConfig
config = defaultEngineConfig { initialDocument = fileDocument "/tmp/test.qml" }

main :: IO ()
main = runQMLReact config $ qmlObject =<< do
  timer <- fromAddHandler (callEvery 1000000)
  App
    <$> prop (\App{..} -> accumB 0 $ fmap (+) (behavior increment) <@ timer)
    <*> prop (\App{..} -> stepper 1 $ changed increment)
