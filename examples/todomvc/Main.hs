{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans
import Graphics.QML.Engine
import Prelude
import Reflex
import Reflex.Host.App
import Reflex.QML

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Reflex.QML.Prop as Prop

data TodoItem = TodoItem
  { description :: Text.Text
  , completed :: Bool
  , editing :: Bool
  } deriving Show

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

makeItem :: MonadAppHost t m => Text.Text -> ObjectBuilder m (Event t (), Dynamic t TodoItem)
makeItem desc = mdo
  descriptionD <- Prop.mutableHold "description" desc
  completedD <- Prop.mutableHold "completed" False
  editingD <- Prop.mutableHold "editing" False
  removedE <- void <$> Prop.methodConst "remove" Prop.noResult
  todoD <- $(qDyn
   [| TodoItem
      $(unqDyn [| descriptionD |])
      $(unqDyn [| completedD |])
      $(unqDyn [| editingD |])
   |])
  return (removedE, todoD)

data Removable t a = Removable
  { removableRemove :: Event t ()
  , removableValue :: a
  }

_removableValue :: Applicative f => (a -> f b) -> Removable t a -> f (Removable t b)
_removableValue f (Removable x a) = Removable x <$> f a

listRemovable :: forall t m a. (Reflex t, MonadHold t m, MonadSample t m, MonadFix m)
     => [Removable t a] -> m (Dynamic t [Removable t a])
listRemovable items = do
  let numbered = Map.fromList $ zip [(0 :: Int)..] items
      removeE = void . removableRemove
  dyn <- foldDyn (flip Map.difference) numbered $ mergeMap $ fmap removeE numbered
  mapDyn Map.elems dyn

data TodoFilter = FilterAll | FilterActive | FilterCompleted

checkFilter :: TodoFilter -> TodoItem -> Bool
checkFilter FilterAll = const True
checkFilter FilterActive = not . completed
checkFilter FilterCompleted = completed

filterTodoList :: TodoFilter -> [(a, TodoItem)] -> [(a, TodoItem)]
filterTodoList f = filter $ checkFilter f . snd

joinMapDynList :: MonadAppHost t m => (a -> [Dynamic t b]) -> Dynamic t a -> m (Dynamic t [b])
joinMapDynList f =
  mapDyn (Map.fromList . zip [(0::Int)..] . f) >=> mapDyn Map.elems . joinDynThroughMap

main :: IO ()
main = do
  startEventLoop
  requireEventLoop $ hostQmlApp engineConfig $ Prop.namespace "app" $ mdo
    newItem <- Prop.methodConst "newItem" $ Prop.haskellResult . return

    todoItems <- lift . fmap joinDyn $ holdAppHost (return $ constDyn []) $
     ffor newItem $ \x -> do
      (obj, (removedE, todoD)) <- runObjectBuilder $ makeItem x
      let clearE = flip push clearCompletedE $ \() -> do
            item <- sample $ current todoD
            return $ if completed item then Just () else Nothing
      oldItems <- sample $ current todoItems
      objTodoD <- mapDyn (obj,) todoD
      listRemovable (Removable (leftmost [clearE, removedE]) objTodoD : oldItems)
    allTodoItems <- lift $ joinMapDynList (map removableValue) todoItems

    filteredTodoItems <- combineDyn filterTodoList currentFilter allTodoItems
    Prop.readonly "todos" =<< mapDyn (fmap reverse . traverse fst) filteredTodoItems

    itemsLeft <- mapDyn (pure . length . filterTodoList FilterActive) allTodoItems
    Prop.readonly "itemsLeft" itemsLeft

    todoFilter <- Prop.namespace "filter" $ do
      completedE <- Prop.methodConst "completed" Prop.noResult
      allE       <- Prop.methodConst "all" Prop.noResult
      activeE    <- Prop.methodConst "active" Prop.noResult
      pure $ mergeWith (const . const $ FilterAll)
       [ FilterCompleted <$ completedE
       , FilterActive    <$ activeE
       , FilterAll       <$ allE
       ]
    currentFilter <- holdDyn FilterAll todoFilter

    clearCompletedE <- Prop.methodConst "clearCompleted" Prop.noResult
    return ()
