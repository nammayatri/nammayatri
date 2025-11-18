module SharedLogic.PaymentType where

import Kernel.Prelude

data CumulativeOfferResp = CumulativeOfferResp
  { offerTitle :: Text,
    offerDescription :: Text,
    offerSponsoredBy :: [Text],
    offerIds :: [Text]
  }
  deriving (Generic, Show, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
