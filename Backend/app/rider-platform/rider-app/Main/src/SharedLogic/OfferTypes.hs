module SharedLogic.OfferTypes where

import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)

data CumulativeOfferResp = CumulativeOfferResp
  { offerTitle :: Text,
    offerDescription :: Text,
    offerSponsoredBy :: [Text],
    offerIds :: [Text],
    offerListResp :: [OfferRespAPIEntity],
    offerStyle :: Maybe OfferThemeConfig
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- Client falls back to its hardcoded styling for any absent field, so every
-- field stays Maybe and absence must never be treated as an error.
data OfferStyle = OfferStyle
  { backgroundColor :: Maybe Text,
    borderColor :: Maybe Text,
    iconColor :: Maybe Text,
    titleColor :: Maybe Text,
    subtitleColor :: Maybe Text,
    iconUrl :: Maybe Text
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OfferThemeConfig = OfferThemeConfig
  { light :: Maybe OfferStyle,
    dark :: Maybe OfferStyle
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
