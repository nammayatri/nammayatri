module Domain.Types.RentalsIntercityCache where

import Database.Beam.Backend
import EulerHS.Prelude hiding (elem, minimumBy)
import Kernel.External.Maps.Types
import Kernel.Prelude hiding (map, minimumBy)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Utils.Common
import Tools.Beam.UtilsTH (mkBeamInstancesForList)

data FareCacheReq = FareCacheReq
  { currentCity :: Maybe City.City,
    currentLatLong :: LatLong
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data FareCacheResp = FareCacheResp
  { interCityMinimumFareResp :: Maybe [IntercitySearchResp],
    rentalsMininumFareResp :: Maybe [RentalsSearchResp]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data IntercitySearchResp = IntercitySearchResp
  { minimumFare :: Maybe HighPrecMoney,
    destinationItem :: Maybe IntercitySearchLocation
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data RentalsSearchResp = RentalsSearchResp
  { minimumFare :: Maybe HighPrecMoney,
    rentalElement :: RentalsConfig
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data RentalsConfig = RentalsConfig
  { rentalDuration :: Seconds,
    rentalDistance :: Meters,
    rentalPriority :: Int,
    rentalImageUrl :: Maybe Text
  }
  deriving stock (Generic, Show, Read, Ord, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IntercitySearchLocation = IntercitySearchLocation
  { destination :: Maybe LatLong,
    destinationCity :: Maybe Text,
    destinationCityBannerImageUrl :: Maybe Text,
    destinationCityButtonImageUrl :: Maybe Text,
    destinationDistance :: Maybe Meters,
    destinationDuration :: Maybe Seconds
  }
  deriving stock (Generic, Show, Read, Ord, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be IntercitySearchLocation where
  sqlValueSyntax = autoSqlValueSyntax

$(mkBeamInstancesForList ''IntercitySearchLocation)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be RentalsConfig where
  sqlValueSyntax = autoSqlValueSyntax

$(mkBeamInstancesForList ''RentalsConfig)
