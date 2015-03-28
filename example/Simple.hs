{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Concurrent
import Control.Monad
import GHC.Generics
import Graphics.QML.Engine
import Graphics.QML.React
import Reactive.Banana
import Reactive.Banana.Frameworks

data StateView p = StateView
  { statx    :: Member Mut Int p
  , staty    :: Member Mut Int p
  , computed :: Member View Int p
  } deriving Generic1
instance QObject StateView

stateView :: Frameworks t => Int -> Int -> Moment t (Def t StateView)
stateView x y = StateView
  <$> prop (stepper x . changed . statx)
  <*> prop (stepper y . changed . staty)
  <*> prop (\StateView{..} -> (+) <$> behavior statx <*> behavior staty)

data App t = App
  { turn :: Member Mut Int t
  , states :: Member Var [[(Int, Int)]] t
--  , currentState :: Member View [StateView T] t
  , test :: Member Fun (Int -> IO Int) t
  } deriving Generic1
instance QObject App

config :: EngineConfig
config = defaultEngineConfig { initialDocument = fileDocument "/tmp/test.qml" }

main :: IO ()
main = runQMLReact config $ qmlObject =<< do
  App
    <$> prop (\App{..} -> stepper 1 $ result test)
    <*> prop (\App{..} -> pure [])
    <*> prop (\App{..} -> fmap fun $ behavior turn)
 where
  fun t t' = t' <$ putStrLn ("test: " ++ show t ++ ":" ++ show t')
