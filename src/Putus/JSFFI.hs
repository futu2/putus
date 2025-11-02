module Putus.JSFFI where

import GHC.Wasm.Prim

foreign import javascript unsafe "document.getElementById($1)"
  js_document_getElementById :: JSString -> IO JSVal

foreign import javascript unsafe "$1.parentElement"
  js_parentElement :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.remove()"
  js_remove :: JSVal -> IO ()

foreign import javascript unsafe "document.createElement($1)"
  js_document_createElement :: JSString -> IO JSVal

foreign import javascript unsafe "$1.textContent = $2"
  js_setTextContent :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.append($2)"
  js_append :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.replaceChildren()"
  js_replaceChildren :: JSVal -> IO ()

foreign import javascript unsafe "$1.target.value"
  js_event_target_value :: JSVal -> IO Double

foreign import javascript unsafe "$1.style.opacity = $2"
  js_setOpacity :: JSVal -> Double -> IO ()

foreign import javascript unsafe "$1.addEventListener($2, $3)"
  js_addEventListener :: JSVal -> JSString -> JSVal -> IO ()

foreign import javascript "wrapper"
  asEventListener :: (JSVal -> IO ()) -> IO JSVal

foreign import javascript unsafe "$1.setAttribute($2, $3);"
  js_setAttribute :: JSVal -> JSString -> JSString -> IO ()

foreign import javascript unsafe "document.createElement('div')"
  js_document_createElement_div :: IO JSVal

foreign import javascript unsafe "document.createElement('small')"
  js_document_createElement_small :: IO JSVal

foreign import javascript unsafe "document.createElement('h1')"
  js_document_createElement_h1 :: IO JSVal

foreign import javascript unsafe "document.createElement('h2')"
  js_document_createElement_h2 :: IO JSVal

foreign import javascript unsafe "document.createElement('h3')"
  js_document_createElement_h3 :: IO JSVal

foreign import javascript unsafe "document.createElement('h4')"
  js_document_createElement_h4 :: IO JSVal

foreign import javascript unsafe "document.createElement('h5')"
  js_document_createElement_h5 :: IO JSVal

foreign import javascript unsafe "document.createElement('p')"
  js_document_createElement_p :: IO JSVal

foreign import javascript unsafe "document.createElement('span')"
  js_document_createElement_span :: IO JSVal

foreign import javascript unsafe "document.createElement('input')"
  js_document_createElement_input :: IO JSVal

foreign import javascript unsafe "document.createElement('button')"
  js_document_createElement_button :: IO JSVal

foreign import javascript unsafe "document.createElement('li')"
  js_document_createElement_li :: IO JSVal

foreign import javascript unsafe "document.createElement('ul')"
  js_document_createElement_ul :: IO JSVal

foreign import javascript unsafe "document.createElement('ol')"
  js_document_createElement_ol :: IO JSVal

foreign import javascript unsafe "document.createElement('hgroup')"
  js_document_createElement_hgroup :: IO JSVal

foreign import javascript unsafe "document.createElement('form')"
  js_document_createElement_form :: IO JSVal

foreign import javascript unsafe "document.createElement('nav')"
  js_document_createElement_nav :: IO JSVal

foreign import javascript unsafe "document.createElement('dialog')"
  js_document_createElement_dialog :: IO JSVal

foreign import javascript unsafe "document.createElement('header')"
  js_document_createElement_header :: IO JSVal

foreign import javascript unsafe "document.createElement('fieldset')"
  js_document_createElement_fieldset :: IO JSVal

foreign import javascript unsafe "document.createElement('main')"
  js_document_createElement_main :: IO JSVal

foreign import javascript unsafe "document.createElement('section')"
  js_document_createElement_section :: IO JSVal

foreign import javascript unsafe "document.createElement('blockquote')"
  js_document_createElement_blockquote :: IO JSVal

foreign import javascript unsafe "document.createElement('figure')"
  js_document_createElement_figure :: IO JSVal

foreign import javascript unsafe "document.createElement('figcaption')"
  js_document_createElement_figcaption :: IO JSVal

foreign import javascript unsafe "document.createElement('label')"
  js_document_createElement_label :: IO JSVal

foreign import javascript unsafe "document.createElement('details')"
  js_document_createElement_details :: IO JSVal

foreign import javascript unsafe "document.createElement('summary')"
  js_document_createElement_summary :: IO JSVal

foreign import javascript unsafe "document.createElement('article')"
  js_document_createElement_article :: IO JSVal

foreign import javascript unsafe "document.createElement('footer')"
  js_document_createElement_footer :: IO JSVal

foreign import javascript unsafe "document.createElement('progress')"
  js_document_createElement_progress :: IO JSVal
