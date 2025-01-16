{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.PriceBreakup where

import Data.OpenApi (ToSchema)
import qualified Domain.Action.UI.Quote
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth

newtype QuoteBreakupRes = QuoteBreakupRes {quoteBreakup :: [Domain.Action.UI.Quote.QuoteBreakupAPIEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
