module SharedLogic.PaymentType where

import Data.Default.Class
import Kernel.Prelude

data CumulativeOfferResp = CumulativeOfferResp
  { offerTitle :: Text,
    offerDescription :: Text,
    offerSponsoredBy :: [Text],
    offerIds :: [Text]
  }
  deriving (Generic, Show, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Default CumulativeOfferResp where
  def =
    CumulativeOfferResp
      { offerTitle = "Offer Title",
        offerDescription = "Offer Description",
        offerSponsoredBy = [],
        offerIds = []
      }
