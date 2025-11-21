module Putus.Dom where

import GHC.Wasm.Prim
import Putus.Core
import Putus.JSFFI
import Control.Monad.Reader

removeChildren :: Component m => m ()
removeChildren = do
  root <- asks currentElement
  liftIO $ js_replaceChildren root

getElementById :: MonadIO m => String -> m JSVal
getElementById = liftIO . js_document_getElementById . toJSString 

setTextContent :: Component m => String -> m ()
setTextContent textContent = do
  root <- asks currentElement
  liftIO $ js_setTextContent root (toJSString textContent)

text_ :: Component m => String -> m ()
text_ = setTextContent

append :: (MonadIO m) => JSVal -> JSVal -> m ()
append r child = liftIO $ js_append r child

insertBefore :: (MonadIO m) => JSVal -> JSVal -> JSVal -> m ()
insertBefore r newChild refChild = liftIO $ js_insertBefore r newChild refChild

remove :: (MonadIO m) => JSVal -> m ()
remove = liftIO . js_remove

children :: (MonadIO m) => JSVal -> m JSVal
children = liftIO . js_children

getIndex :: (MonadIO m) => Int -> JSVal -> m JSVal
getIndex i xs = liftIO $ js_getIndex i xs

createElement :: MonadIO m => String -> m JSVal
createElement = liftIO . js_document_createElement . toJSString 

addEventListener :: Component m => String -> (JSVal -> IO ()) -> m ()
addEventListener eventName eventListener = do
  root <- asks currentElement
  liftIO $ do
    callback <- asEventListener eventListener
    js_addEventListener root (toJSString eventName) callback

setAttribute :: Component m => String -> String -> m ()
setAttribute attributeName attributeValue = do
  root <- asks currentElement
  liftIO $ js_setAttribute root (toJSString attributeName) (toJSString attributeValue)

el :: Component m => String -> m () -> m ()
el tag child = do
  newElement <- createElement tag
  root <- asks currentElement
  local (\ x -> x {currentElement = newElement}) child
  append root newElement

wrapRawElement :: Component m => IO JSVal -> m () -> m ()
wrapRawElement raw child = do
  newElement <- liftIO raw
  root <- asks currentElement
  ref <- asks insertRef
  local (\ x -> x {currentElement = newElement, insertRef = Nothing}) child
  case ref of
    Nothing -> append root newElement
    Just refChild -> insertBefore root newElement refChild

-- events

onClick :: Component m => Signal m a -> (a -> a) -> m ()
onClick sig f = addEventListener "click" (const $ updateSignal sig f)

onClickIO :: Component m => Signal m a -> IO (a -> a) -> m ()
onClickIO sig f = addEventListener "click" (const $ updateSignalIO sig f)

class FromEventValue a where
  fromEventValue :: JSVal -> IO a

instance FromEventValue Bool where
  fromEventValue = js_event_target_value_bool

instance FromEventValue Double where
  fromEventValue = js_event_target_value_double

instance FromEventValue String where
  fromEventValue = fmap fromJSString . js_event_target_value_string

onChange :: (Component m, FromEventValue b) => Signal m a -> (b -> a -> a) -> m ()
onChange sig f = addEventListener "change" $ \e -> do
  e' <- fromEventValue e
  updateSignal sig $ f e'

onInput :: (Component m, FromEventValue b) => Signal m a -> (b -> a -> a) -> m ()
onInput sig f = addEventListener "input" $ \e -> do
  e' <- fromEventValue e
  updateSignal sig $ f e'

onChangeChecked :: Component m => Signal m a -> (Bool -> a -> a) -> m ()
onChangeChecked sig f = addEventListener "change" $ \e -> do
  e' <- js_event_target_checked e
  updateSignal sig $ f e'
