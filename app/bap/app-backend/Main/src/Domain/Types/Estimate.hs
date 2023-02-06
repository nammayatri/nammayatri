{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Estimate where

import Beckn.External.Maps
import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.GenericPretty
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant (VehicleVariant)
import Servant.API

data Estimate = Estimate
  { id :: Id Estimate,
    requestId :: Id DSearchRequest.SearchRequest,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    totalFareRange :: FareRange,
    providerId :: Text,
    providerUrl :: BaseUrl,
    providerName :: Text,
    providerMobileNumber :: Text,
    providerCompletedRidesCount :: Int,
    vehicleVariant :: VehicleVariant,
    tripTerms :: Maybe DTripTerms.TripTerms,
    createdAt :: UTCTime,
    estimateBreakupList :: [EstimateBreakup],
    nightShiftRate :: Maybe NightShiftRate,
    status :: Maybe EstimateStatus,
    updatedAt :: UTCTime,
    waitingCharges :: WaitingCharges,
    driversLocation :: [LatLong]
  }
  deriving (Generic, Show)

deriving instance Read LatLong

data NightShiftRate = NightShiftRate
  { nightShiftMultiplier :: Maybe Centesimal,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EstimateBreakup = EstimateBreakup
  { id :: Id EstimateBreakup,
    estimateId :: Id Estimate,
    title :: Text,
    price :: EstimateBreakupPrice
  }
  deriving (Generic, FromJSON, ToJSON, Show, PrettyShow, ToSchema)

data EstimateBreakupPrice = EstimateBreakupPrice
  { currency :: Text,
    value :: Money
  }
  deriving (Generic, FromJSON, ToJSON, Show, PrettyShow, ToSchema)

data FareRange = FareRange
  { minFare :: Money,
    maxFare :: Money
  }
  deriving (Generic, Show, PrettyShow, ToJSON, FromJSON, ToSchema)

data EstimateAPIEntity = EstimateAPIEntity
  { id :: Id Estimate,
    vehicleVariant :: VehicleVariant,
    estimatedFare :: Money,
    estimatedTotalFare :: Money,
    discount :: Maybe Money,
    totalFareRange :: FareRange,
    agencyName :: Text,
    agencyNumber :: Text,
    agencyCompletedRidesCount :: Int,
    tripTerms :: [Text],
    createdAt :: UTCTime,
    estimateFareBreakup :: [EstimateBreakupAPIEntity],
    nightShiftRate :: Maybe NightShiftRate,
    waitingCharges :: WaitingCharges,
    driversLatLong :: [LatLong]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data WaitingCharges = WaitingCharges
  { waitingTimeEstimatedThreshold :: Maybe Seconds,
    waitingChargePerMin :: Maybe Money
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EstimateBreakupAPIEntity = EstimateBreakupAPIEntity
  { title :: Text,
    price :: Money
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

mkEstimateAPIEntity :: Estimate -> EstimateAPIEntity
mkEstimateAPIEntity Estimate {..} = do
  EstimateAPIEntity
    { agencyName = providerName,
      agencyNumber = providerMobileNumber,
      agencyCompletedRidesCount = providerCompletedRidesCount,
      tripTerms = fromMaybe [] $ tripTerms <&> (.descriptions),
      estimateFareBreakup = mkEstimateBreakupAPIEntity <$> estimateBreakupList,
      driversLatLong = driversLocation,
      ..
    }

mkEstimateBreakupAPIEntity :: EstimateBreakup -> EstimateBreakupAPIEntity
mkEstimateBreakupAPIEntity EstimateBreakup {..} = do
  EstimateBreakupAPIEntity
    { title = title,
      price = price.value
    }

data EstimateStatus = NEW | DRIVER_QUOTE_REQUESTED | CANCELLED | GOT_DRIVER_QUOTE | DRIVER_QUOTE_CANCELLED | COMPLETED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromHttpApiData EstimateStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData EstimateStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode
