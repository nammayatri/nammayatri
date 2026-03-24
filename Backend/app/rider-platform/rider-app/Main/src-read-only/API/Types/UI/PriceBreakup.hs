{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.PriceBreakup where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Domain.Action.UI.Quote



newtype QuoteBreakupRes
  = QuoteBreakupRes {quoteBreakup :: [Domain.Action.UI.Quote.QuoteBreakupAPIEntity]}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



