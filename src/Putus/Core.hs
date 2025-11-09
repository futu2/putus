module Putus.Core where

import Control.Monad
import Control.Concurrent
import GHC.Wasm.Prim
import Data.Foldable
import Control.Monad.Reader
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

-- |Context of component, contains an element all operation acts on
data ComponentConfig = ComponentConfig {currentElement :: JSVal}

type Component m = (MonadReader ComponentConfig m, MonadIO m, MonadUnliftIO m)

data Signal m a = Signal
  { signalValue :: MVar a
  , signalChan :: Chan (a -> a)
  , signalHandlers :: MVar [a -> m ()]
  }

forkApp :: (MonadUnliftIO m) => m () -> m ()
forkApp m = void $ withRunInIO $ \run -> forkIO $ run m

-- | Create a new signala and use it in scope
withSignal :: Component m => a -> (Signal m a -> m ()) -> m ()
withSignal initValue inner = do
  sigValue <- liftIO $ newMVar initValue
  sig <- liftIO $ newChan
  handlers :: MVar [a -> m ()] <- liftIO $ newMVar []

  _ <- forkApp $ forever $ do
    c <- liftIO $ readChan sig
    v0 <- liftIO $ takeMVar sigValue
    let v1 = c v0
    liftIO $ putMVar sigValue v1
    fs <- liftIO $ readMVar handlers
    for_ fs (\f -> (f v1))

  inner $ Signal 
    { signalValue = sigValue
    , signalChan = sig
    , signalHandlers = handlers
    }

  fs0 <- liftIO $ readMVar handlers
  v00 <- liftIO $ readMVar sigValue
  for_ fs0 (\f -> (f v00))

readSignal :: Signal m a -> IO a
readSignal = readMVar . signalValue

updateSignal :: Signal m a -> (a -> a) -> IO ()
updateSignal = writeChan . signalChan

use :: Component m => Signal m a -> (a -> m ()) -> m ()
use sig h = do
  config0 <- ask
  liftIO $ modifyMVar_ (signalHandlers sig) (pure . ((\a0 -> local (const config0) (h a0)) :)) 
