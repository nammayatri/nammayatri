module SharedLogic.PaymentType where

import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.Prelude

data CumulativeOfferResp = CumulativeOfferResp
  { offerTitle :: Text,
    offerDescription :: Text,
    offerSponsoredBy :: [Text],
    offerIds :: [Text]
  }
  deriving (Generic, Show, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CumulativeOfferReq extra = CumulativeOfferReq
  { offerListResp :: Payment.OfferListResp,
    extraParams :: extra
  }
  deriving (Generic, Show, FromJSON, ToJSON)
