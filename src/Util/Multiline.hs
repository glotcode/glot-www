module Util.Multiline
    ( multiline
    ) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as Quote
import qualified Prelude


multiline :: Quote.QuasiQuoter
multiline = Quote.QuasiQuoter
    { Quote.quoteExp = TH.stringE
    , Quote.quotePat  = \_ -> Prelude.fail "illegal raw string QuasiQuote (allowed as expression only, used as a pattern)"
    , Quote.quoteType = \_ -> Prelude.fail "illegal raw string QuasiQuote (allowed as expression only, used as a type)"
    , Quote.quoteDec  = \_ -> Prelude.fail "illegal raw string QuasiQuote (allowed as expression only, used as a declaration)"
    }
