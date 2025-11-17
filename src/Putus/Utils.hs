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

reactKeyed :: (Component m, Ord k) => Signal m [(k,a)] -> (a -> m ()) -> m ()
reactKeyed sig h = do
  lastRenderStore <- liftIO $ newMVar ([] :: [(k, a)])
  use sig $ \vs -> do
    lastRender <- liftIO $ readMVar lastRenderStore
    let newKeys = fst <$> vs
    let oldKeys = fst <$> lastRender
    let moves = minimalMoves oldKeys newKeys
    c <- asks currentElement
    let nthChild i = getIndex i =<< children c
    for_ moves $ \moveDetail -> case moveDetail of
      Delete i -> case findIndex (== i) oldKeys of
        Just kk -> remove =<< nthChild kk
        Nothing -> pure ()
      _ -> pure ()
    liftIO $ putMVar lastRenderStore vs
    
runApp :: String -> ReaderT ComponentConfig IO () -> IO ()
runApp rootId mainApp = void $ do
  appRoot <- getElementById rootId
  appRoot' <- js_parentElement appRoot
  js_remove appRoot
  runReaderT mainApp (ComponentConfig appRoot')
