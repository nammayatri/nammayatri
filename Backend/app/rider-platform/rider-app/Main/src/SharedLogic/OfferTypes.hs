module SharedLogic.OfferTypes where

import qualified Data.Aeson
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import qualified Lib.Payment.Domain.Types.Offer as DOffer

data CumulativeOfferResp = CumulativeOfferResp
  { offerTitle :: Text,
    offerDescription :: Text,
    offerSponsoredBy :: [Text],
    offerIds :: [Text],
    offerListResp :: [OfferRespAPIEntity],
    -- Opaque client-owned payload (e.g. offerStyle theming): the backend
    -- passes it through from CUMULATIVE_OFFER_POLICY untouched so new client
    -- fields never need a backend change.
    metadata :: Maybe Data.Aeson.Value,
    promoCard :: Maybe PromoCard
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PromoCard = PromoCard
  { header :: Maybe Text,
    title :: Maybe Text,
    subtitle :: Maybe Text,
    subtitleInfo :: Maybe Text,
    imageUrl :: Maybe Text,
    lottieUrl :: Maybe Text,
    textColor :: Maybe Text,
    bgGradientColors :: Maybe [Text]
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
    estimatedPostOfferAmount :: HighPrecMoney,
    offerType :: Maybe DOffer.OfferType,
    minimumAmount :: Maybe HighPrecMoney,
    frequencyType :: Maybe DOffer.OfferFrequency,
    appliedCount :: Maybe Int,
    maxApplyCount :: Maybe Int
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
