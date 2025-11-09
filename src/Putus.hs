{-|
Module      : Putus
Description : A reactive frontend framework via ghc wasm jsffi
Copyright   : (c) futu, 2025
License     : BSD-2-Clause-Patent
Maintainer  : futu@email.com
Stability   : experimental
Portability : POSIX
-}
module Putus 
(
-- * Intro
{- |
This module aims at providing lightweight api,
while keeping things simple and not over optimized.

Putus is a Haskell Wasm framework for making interactive web applications.
With Putus, you can use your existing Haskell knowledge to build components that can be reused throughout your app.
Putus provides the tools to enhance your components with reactivity:
Monadic EDSL code that links the user interface with the data that it uses and creates.
-}

-- * Quick View
{-|
In your index.html, create an element with id "app".
And make your Main.hs which compiles to an wasm reactor module

@

module Main where

import Putus

main :: IO ()
main = undefined

foreign export javascript "setup" setup :: IO ()

setup :: IO ()
setup = do
  runApp "app" $ do
    header_ $ do
      class_ "container"
      hgroup_ $ do
        h1_ $ text_ "GHC Wasm"
        p_ $ text_ "using jsffi to manipulate dom"
    main_ $ do
      class_ "container"
      section_ $ do
        p_ $ text_ "Hello World from GHC Wasm"
@
-}

-- * How to setup ghc-wasm environment
{-|
see <https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta>
-}

-- * Re-exports
  module Putus.Core
, module Putus.Dom
, module Putus.Html5
, module Putus.Utils
)
where

import Putus.Core
import Putus.Dom
import Putus.Html5
import Putus.Utils


