module Domain.Types.DriverOffer where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.GenericPretty
import qualified Domain.Types.Estimate as DEstimate
import Types.Common

data DriverOffer = DriverOffer
  { id :: Id DriverOffer,
    estimateId :: Id DEstimate.Estimate,
    driverName :: Text,
    durationToPickup :: Int, -- Seconds?
    distanceToPickup :: HighPrecMeters,
    validTill :: UTCTime,
    bppQuoteId :: Id BPPQuote,
    rating :: Maybe Double
  }
  deriving (Generic, Show, PrettyShow)

data DriverOfferAPIEntity = DriverOfferAPIEntity
  { driverName :: Text,
    durationToPickup :: Int, -- Seconds?
    distanceToPickup :: HighPrecMeters,
    validTill :: UTCTime,
    rating :: Maybe Double
  }
  deriving (Generic, Show, PrettyShow, ToJSON, FromJSON, ToSchema)
