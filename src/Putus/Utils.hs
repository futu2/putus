module Putus.Utils where

import Putus.Core
import Putus.Dom
import Putus.Html5
import Control.Monad.Reader
import Control.Monad
import Putus.JSFFI
import Control.Concurrent
import Patience
import Data.List
import Data.Foldable

import Data.Map (Map)
import qualified Data.Map as Map

data Op a = Delete a | InsertBefore a a | Append a | InsertBeforeNew a a | AppendNew a
  deriving Show

minimalMoves :: (Ord a) => [a] -> [a] -> [Op a]
minimalMoves src tgt = go diffResult
  where
    diffResult = diff src tgt

    isBoth :: Item a -> Bool
    isBoth (Both _ _) = True
    isBoth _ = False

    go ((New i) : xs) = case find isBoth xs of
        Just (Both k _) -> (if elem i src then InsertBefore i k else InsertBeforeNew i k) : go xs
        otherwise -> (if elem i src then Append i else AppendNew i) : go xs
    go ((Old i) : xs) = if elem i tgt then go xs else (Delete i) : go xs
    go (_ : xs) = go xs
    go [] = []

react :: Component m => Signal m a -> (a -> m ()) -> m ()
react sig h = div_ $ use sig (\v -> removeChildren >> h v)

reactIn :: Component m => Signal m a -> (a -> m ()) -> m ()
reactIn sig h = use sig (\v -> removeChildren >> h v)

reactKeyed :: (Component m, Ord k, Foldable t, Eq a) => Signal m (t a) -> (a -> k) -> (Signal m a -> m ()) -> m ()
reactKeyed sig keyBy h = do
  keyOrderStore <- liftIO $ newMVar ([] :: [k])
  keySignalStore <- liftIO $ newMVar (Map.empty :: Map k (Signal m a))
  use sig $ \vs' -> do
    let vs = toList vs'
    oldKeys <- liftIO $ takeMVar keyOrderStore
    oldSignals <- liftIO $ readMVar keySignalStore
    let newKeys = keyBy <$> vs
    let moves = minimalMoves oldKeys newKeys

    for_ vs $ \v -> case Map.lookup (keyBy v) oldSignals of
      Nothing -> pure ()
      Just sig -> liftIO $ do
        vOld <- readSignal sig
        when (vOld /= v) $ updateSignal sig (const v)

    c <- asks currentElement
    currentChild <- children c
    currentChildCount <- childElementCount c
    childList <- traverse (\i -> getIndex i currentChild) [0..currentChildCount-1]
    -- let nthChild i = getIndex i currentChild
    for_ moves $ \moveDetail -> case moveDetail of
      Delete j -> case findIndex (== j) oldKeys of
        Nothing -> error "impossible"
        Just kk -> do
          remove (childList !! kk)
          liftIO $ modifyMVar_ keySignalStore (pure . Map.delete j)
      Append i -> case findIndex (== i) oldKeys of
        Nothing -> error "impossible"
        Just kk -> do
          append c (childList !! kk)

      InsertBefore i j -> case (findIndex (== i) oldKeys, findIndex (== j) oldKeys) of
        (Just kk1, Just kk2) -> do
          let moveChild = childList !! kk1
          let refChild = childList !! kk2
          insertBefore c moveChild refChild
        _ -> error "impossible"
     
      AppendNew i -> case find ((== i) . keyBy) vs of
        Nothing -> error "impossible"
        Just v -> do
          withSignal v $ \sSignal -> do
            liftIO $ modifyMVar_ keySignalStore (pure . Map.insert i sSignal)
            h sSignal

      InsertBeforeNew i j -> case findIndex (== j) oldKeys of
        Nothing -> error "impossible"
        Just kk -> do
          let refChild = childList !! kk
          case find ((== i) . keyBy) vs of
            Nothing -> error "impossible"
            Just v -> do
              withSignal v $ \sSignal -> do
                liftIO $ modifyMVar_ keySignalStore (pure . Map.insert i sSignal)
                local (\x -> x {insertRef = Just refChild}) $ h sSignal
      _ -> pure ()
    liftIO $ putMVar keyOrderStore newKeys
    

runApp :: String -> ReaderT ComponentConfig IO () -> IO ()
runApp rootId mainApp = void $ do
  appRoot <- getElementById rootId
  appRoot' <- js_parentElement appRoot
  js_remove appRoot
  runReaderT mainApp (ComponentConfig {currentElement = appRoot', insertRef = Nothing})
