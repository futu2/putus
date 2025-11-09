# Putus

![Logo](doc/logo.png)

a simple reactive framework in haskell using ghc wasm js ffi

## Example

### Hello World

```haskell
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
```

