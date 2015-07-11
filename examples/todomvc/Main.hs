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
import Reflex.QML.Internal.AppHost
import Reflex.QML.Internal.Object
import Reflex.QML.Internal.Run

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

makeItem :: MonadAppHost t m => Text.Text -> ObjectT m (Event t (), Dynamic t TodoItem)
makeItem desc = mdo
  descriptionD <- Prop.mutableHold "description" desc
  completedD <- Prop.mutableHold "completed" False
  editingD <- Prop.mutableHold "editing" False
  removedE <- void <$> Prop.method "remove" (constDyn ((), ()))
  todoD <- $(qDyn
   [| TodoItem
      $(unqDyn [| descriptionD |])
      $(unqDyn [| completedD |])
      $(unqDyn [| editingD |])
   |])
  return (removedE, todoD)

data Removable t a = Removable
  { removableAlive :: Dynamic t Bool
  , removableValue :: a
  }

_removableValue :: Applicative f => (a -> f b) -> Removable t a -> f (Removable t b)
_removableValue f (Removable x a) = Removable x <$> f a

list :: forall t m a. (Reflex t, MonadHold t m, MonadSample t m, MonadFix m)
     => [Removable t a] -> m (Dynamic t [Removable t a])
list items = do
  let numbered = Map.fromList $ zip [(0 :: Int)..] items
      removeE = void . updated . removableAlive
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
    newItem <- Prop.methodVoid "newItem" . constDyn $ \x -> (x,())
    todoItemsM <- lift . fmap joinDyn $ holdAppHost (return $ constDyn []) $
     ffor newItem $ \x -> do
      (activate, (obj, (removedE, todoD))) <- collectPostActions . runObjectT $ makeItem x
      let clearE = flip push clearCompletedE $ \() -> do
            item <- sample $ current todoD
            return $ if completed item then Just () else Nothing
      alive <- holdDyn True $ False <$ leftmost [removedE, clearE]
      oldItems <- sample $ current todoItemsM
      objTodoD <- mapDyn (obj,) todoD
      list (Removable alive (objTodoD <$ performPostBuild_ activate) : oldItems)
    todoDItems <- lift $ holdAppHost (return []) $
      mapM (_removableValue id) <$> updated todoItemsM
    allTodoItems <- lift $ joinMapDynList (map removableValue) todoDItems
    lift . performEvent_ $ liftIO . print . map snd <$> updated allTodoItems
    filteredTodoItems <- combineDyn filterTodoList currentFilter allTodoItems
    Prop.readonly "todos" =<< mapDyn (reverse . map fst) filteredTodoItems

    itemsLeft <- mapDyn (length . filterTodoList FilterActive) allTodoItems
    Prop.readonly "itemsLeft" itemsLeft

    todoFilter <- Prop.namespace "filter" $ do
      completedE <- Prop.methodVoid "completed" $ constDyn ((),())
      allE       <- Prop.methodVoid "all" $ constDyn ((),())
      activeE    <- Prop.methodVoid "active" $ constDyn ((),())
      pure $ mergeWith (const . const $ FilterAll)
       [ FilterCompleted <$ completedE
       , FilterActive    <$ activeE
       , FilterAll       <$ allE
       ]
    currentFilter <- holdDyn FilterAll todoFilter

    clearCompletedE <- Prop.methodVoid "clearCompleted" $ constDyn ((), ())
    return ()
