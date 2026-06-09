module SharedLogic.OfferTypes where

import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)

data CumulativeOfferResp = CumulativeOfferResp
  { offerTitle :: Text,
    offerDescription :: Text,
    offerSponsoredBy :: [Text],
    offerIds :: [Text],
    offerListResp :: [OfferRespAPIEntity]
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OfferRespAPIEntity = OfferRespAPIEntity
  { offerId :: Text,
    offerTitle :: Maybe Text,
    offerDescription :: Maybe Text,
    offerTnc :: Maybe Text,
    offerSponsoredBy :: Maybe Text,
    offerCode :: Text,
    autoApply :: Bool,
    isHidden :: Bool,
    amountSaved :: HighPrecMoney,
    postOfferAmount :: HighPrecMoney,
    estimatedAmountSaved :: HighPrecMoney,
    estimatedPostOfferAmount :: HighPrecMoney
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
