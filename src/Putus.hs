module Putus where

import Control.Monad
import Control.Concurrent
import GHC.Wasm.Prim
import Data.Foldable
import Control.Monad.Reader
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

import Putus.JSFFI

data Config = Config {rootElement :: JSVal}

type AppM m = (MonadReader Config m, MonadIO m, MonadUnliftIO m)

forkApp :: (MonadUnliftIO m) => m () -> m ThreadId
forkApp m = withRunInIO $ \run -> forkIO $ run m

data Signal m a = Signal
  { signalValue :: MVar a
  , signalChan :: Chan (a -> a)
  , signalHandlers :: MVar [a -> m ()]
  }

readSignal :: Signal m a -> IO a
readSignal = readMVar . signalValue

updateSignal :: Signal m a -> (a -> a) -> IO ()
updateSignal = writeChan . signalChan

useSignal :: AppM m => Signal m a -> (a -> m ()) -> m ()
useSignal sig h = do
  config0 <- ask
  liftIO $ modifyMVar_ (signalHandlers sig) (pure . ((\a0 -> local (const config0) (h a0)) :)) 

reactSignal :: AppM m => Signal m a -> (a -> m ()) ->m ()
reactSignal sig h = div_ $ useSignal sig (\v -> removeChildren >> h v)

onClick :: AppM m => Signal m a -> (a -> a) -> m ()
onClick sig f = addEventListener "click" (const $ updateSignal sig f)

class FromEventValue a where
  fromEventValue :: JSVal -> IO a

instance FromEventValue Bool where
  fromEventValue = js_event_target_value_bool

instance FromEventValue Double where
  fromEventValue = js_event_target_value_double

instance FromEventValue String where
  fromEventValue = fmap fromJSString . js_event_target_value_string

onChange :: (AppM m, FromEventValue b) => Signal m a -> (b -> a -> a) -> m ()
onChange sig f = addEventListener "change" $ \e -> do
  e' <- fromEventValue e
  updateSignal sig $ f e'

onInput :: (AppM m, FromEventValue b) => Signal m a -> (b -> a -> a) -> m ()
onInput sig f = addEventListener "input" $ \e -> do
  e' <- fromEventValue e
  updateSignal sig $ f e'

onChangeChecked :: AppM m => Signal m a -> (Bool -> a -> a) -> m ()
onChangeChecked sig f = addEventListener "change" $ \e -> do
  e' <- js_event_target_checked e
  updateSignal sig $ f e'

-- onChange :: AppM m => Signal m a -> (JSVal -> a -> a) -> m ()
-- onChange sig f = addEventListener "change" (\e -> updateSignal sig $ f e)

withSignal :: AppM m => a -> (Signal m a -> m ()) -> m ()
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

runApp :: String -> ReaderT Config IO () -> IO ()
runApp rootId mainApp = void $ do
  appRoot <- getElementById rootId
  appRoot' <- js_parentElement appRoot
  js_remove appRoot
  runReaderT mainApp (Config appRoot')

removeChildren :: AppM m => m ()
removeChildren = do
  root <- asks rootElement
  liftIO $ js_replaceChildren root

getElementById :: MonadIO m => String -> m JSVal
getElementById = liftIO . js_document_getElementById . toJSString 

setTextContent :: AppM m => String -> m ()
setTextContent textContent = do
  root <- asks rootElement
  liftIO $ js_setTextContent root (toJSString textContent)

text_ :: AppM m => String -> m ()
text_ = setTextContent

append :: (MonadIO m) => JSVal -> JSVal -> m ()
append r child = liftIO $ js_append r child

createElement :: MonadIO m => String -> m JSVal
createElement = liftIO . js_document_createElement . toJSString 

addEventListener :: AppM m => String -> (JSVal -> IO ()) -> m ()
addEventListener eventName eventListener = do
  root <- asks rootElement
  liftIO $ do
    callback <- asEventListener eventListener
    js_addEventListener root (toJSString eventName) callback

setAttribute :: AppM m => String -> String -> m ()
setAttribute attributeName attributeValue = do
  root <- asks rootElement
  liftIO $ js_setAttribute root (toJSString attributeName) (toJSString attributeValue)

class_ :: AppM m => String -> m ()
class_ = setAttribute "class"

type_ :: AppM m => String -> m ()
type_ = setAttribute "type"

role_ :: AppM m => String -> m ()
role_ = setAttribute "role"

el :: AppM m => String -> m () -> m ()
el tag child = do
  newElement <- createElement tag
  root <- asks rootElement
  -- liftIO $ putStrLn tag
  -- liftIO $ debug root
  local (\ x -> x {rootElement = newElement}) child
  append root newElement

wrapRawElement :: AppM m => IO JSVal -> m () -> m ()
wrapRawElement raw child = do
  newElement <- liftIO raw
  root <- asks rootElement
  local (\ x -> x {rootElement = newElement}) child
  append root newElement

div_ :: AppM m => m () -> m ()
div_ = wrapRawElement js_document_createElement_div

h1_ :: AppM m => m () -> m ()
h1_ = wrapRawElement js_document_createElement_h1

h2_ :: AppM m => m () -> m ()
h2_ = wrapRawElement js_document_createElement_h2

h3_ :: AppM m => m () -> m ()
h3_ = wrapRawElement js_document_createElement_h3

h4_ :: AppM m => m () -> m ()
h4_ = wrapRawElement js_document_createElement_h4

h5_ :: AppM m => m () -> m ()
h5_ = wrapRawElement js_document_createElement_h5

p_ :: AppM m => m () -> m ()
p_ = wrapRawElement js_document_createElement_p

small_ :: AppM m => m () -> m ()
small_ = wrapRawElement js_document_createElement_small

span_ :: AppM m => m () -> m ()
span_ = wrapRawElement js_document_createElement_span

button_ :: AppM m => m () -> m ()
button_ = wrapRawElement js_document_createElement_button

input_ :: AppM m => m () -> m ()
input_ = wrapRawElement js_document_createElement_input

header_ :: AppM m => m () -> m ()
header_ = wrapRawElement js_document_createElement_header

main_ :: AppM m => m () -> m ()
main_ = wrapRawElement js_document_createElement_main

section_ :: AppM m => m () -> m ()
section_ = wrapRawElement js_document_createElement_section

progress_ :: AppM m => m () -> m ()
progress_ = wrapRawElement js_document_createElement_progress

ul_ :: AppM m => m () -> m ()
ul_ = wrapRawElement js_document_createElement_ul

li_ :: AppM m => m () -> m ()
li_ = wrapRawElement js_document_createElement_li

hgroup_ :: AppM m => m () -> m ()
hgroup_ = wrapRawElement js_document_createElement_hgroup
