{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Graphics.QML.Engine
import Reflex
import Reflex.QML.Internal

import qualified Data.Text as T
import qualified Reflex.QML.Prop as Prop

engineConfig :: EngineConfig
engineConfig = defaultEngineConfig
  { initialDocument = fileDocument "examples/Games.qml"
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
 requireEventLoop $ runApp engineConfig $ Prop.namespace "app" $
  Prop.namespace "games" $ mdo
   name <- holdDyn ("Example" :: T.Text) nameChanged
   nameChanged <- Prop.mutable "name" name

   won <- holdDyn (0 :: Int) wonChanged
   wonChanged <- Prop.mutable "won" won

   lost <- holdDyn 0 lostChanged
   lostChanged <- Prop.mutable "lost" lost

   total <- Prop.readonly "total" =<< combineDyn (+) won lost
   pure ()
