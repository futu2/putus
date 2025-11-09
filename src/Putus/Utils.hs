module Putus.Utils where

import Putus.Core
import Putus.Dom
import Putus.Html5
import Control.Monad.Reader
import Control.Monad
import Putus.JSFFI

react :: Component m => Signal m a -> (a -> m ()) ->m ()
react sig h = div_ $ use sig (\v -> removeChildren >> h v)

runApp :: String -> ReaderT ComponentConfig IO () -> IO ()
runApp rootId mainApp = void $ do
  appRoot <- getElementById rootId
  appRoot' <- js_parentElement appRoot
  js_remove appRoot
  runReaderT mainApp (ComponentConfig appRoot')
