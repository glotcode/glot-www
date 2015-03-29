module Util.Multiline(
    multiline
) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

multiline :: QuasiQuoter
multiline = QuasiQuoter { quoteExp = stringE }
