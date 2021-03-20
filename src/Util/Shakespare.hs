module Util.Shakespare (
    stextFile
) where

import Prelude
import Language.Haskell.TH
import Text.Shakespeare
import Text.Shakespeare.Text (toText)

settings :: Q ShakespeareSettings
settings = do
  toTExp <- [|toText|]
  wrapExp <- [|id|]
  unWrapExp <- [|id|]
  return $ defaultShakespeareSettings { toBuilder = toTExp
  , wrap = wrapExp
  , unwrap = unWrapExp
  }

stextFile :: FilePath -> Q Exp
stextFile fp = do
  rs <- settings
  shakespeareFile rs { justVarInterpolation = True } fp
