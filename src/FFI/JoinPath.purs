module FFI.JoinPath where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

type FilePath = String

foreign import joinPath :: FilePath -> FilePath -> FilePath

test :: Effect Unit
test = do
  log $ joinPath "abc" "xyz.txt"
