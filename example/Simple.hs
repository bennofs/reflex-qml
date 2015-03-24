module Main where

import Graphics.QML
import Graphics.QML.React

data App p = App
  { counter      :: p View
  , increment    :: p (Mut Int)
  , controls     :: p (Static Controls)
  , incrementNow :: p (Signal a)
  , incremented  :: p (Int -> Int -> Bool)
  } deriving Generic

data Controls p = App
  { running :: p View
  } 

main :: IO ()
main = do
  network <- compile $ do
    timer <- fromAddHandler (callEvery 10)
    obj <- createObject $ \app -> App
      <$> qml (accumB 0 ((+) <$ timer <@> value (increment app)))
      <*> qml (stepper 1 (changed (increment app)))
    
  actuate network
