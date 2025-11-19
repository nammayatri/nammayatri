module SharedLogic.PaymentType where

import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.Prelude
import qualified Lib.JourneyModule.Types as JL

data CumulativeOfferResp = CumulativeOfferResp
  { offerTitle :: Text,
    offerDescription :: Text,
    offerSponsoredBy :: [Text],
    offerIds :: [Text]
  }
  deriving (Generic, Show, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CumulativeOfferReq = CumulativeOfferReq
  { offerListResp :: Payment.OfferListResp,
    extraParams :: [JL.LegInfo]
  }
  deriving (Generic, Show, FromJSON, ToJSON)
