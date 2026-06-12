module SharedLogic.OfferTypes where

import qualified Data.Aeson
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)

data CumulativeOfferResp = CumulativeOfferResp
  { offerTitle :: Text,
    offerDescription :: Text,
    offerSponsoredBy :: [Text],
    offerIds :: [Text],
    offerListResp :: [OfferRespAPIEntity],
    -- Opaque client-owned payload (e.g. offerStyle theming): the backend
    -- passes it through from CUMULATIVE_OFFER_POLICY untouched so new client
    -- fields never need a backend change.
    metadata :: Maybe Data.Aeson.Value
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
