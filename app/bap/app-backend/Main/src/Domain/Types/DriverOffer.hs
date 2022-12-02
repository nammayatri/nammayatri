module Domain.Types.DriverOffer where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.GenericPretty
import qualified Domain.Types.Estimate as DEstimate

data BPPQuote

data DriverOffer = DriverOffer
  { id :: Id DriverOffer,
    estimateId :: Id DEstimate.Estimate,
    driverName :: Text,
    durationToPickup :: Int, -- Seconds?
    distanceToPickup :: HighPrecMeters,
    validTill :: UTCTime,
    bppQuoteId :: Id BPPQuote,
    rating :: Maybe Centesimal
  }
  deriving (Generic, Show, PrettyShow)

data DriverOfferAPIEntity = DriverOfferAPIEntity
  { driverName :: Text,
    durationToPickup :: Int, -- Seconds?
    distanceToPickup :: HighPrecMeters,
    validTill :: UTCTime,
    rating :: Maybe Centesimal
  }
  deriving (Generic, Show, PrettyShow, ToJSON, FromJSON, ToSchema)
