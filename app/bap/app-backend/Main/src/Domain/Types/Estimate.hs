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
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant (VehicleVariant)

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
    driversLatLong :: [LatLong]
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
