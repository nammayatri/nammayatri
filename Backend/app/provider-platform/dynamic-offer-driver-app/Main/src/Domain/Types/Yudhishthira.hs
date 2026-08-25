{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Yudhishthira where

import qualified Data.Aeson as A
import qualified Domain.Types.Booking as SRB
-- import qualified Domain.Types.Estimate as DEst

import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.VehicleVariant as DVV
import Kernel.Prelude
import qualified Lib.Yudhishthira.Types.Application as YA
import qualified Lib.Yudhishthira.Types.Common as YTC
import qualified Lib.Yudhishthira.TypesTH as YTH

data TagData = TagData
  { searchRequest :: DSR.SearchRequest,
    area :: Text,
    specialLocationTag :: Maybe Text,
    specialLocationName :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data EndRideTagData = EndRideTagData
  { ride :: DRide.Ride,
    booking :: SRB.Booking,
    isDriverSameAsCustomer :: Bool,
    shouldBlockCoinsForSameRiderFlow :: Bool,
    rideDurationSeconds :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data SelectTagData = SelectTagData
  { isPetRide :: Bool --,
  -- estimates :: [DEst.Estimate] -------uncomment this line if you want to use estimates in select tag data
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data UpgradeTierTagData = UpgradeTierTagData
  { driverRating :: Maybe Double,
    vehicleAgeInMonths :: Maybe Int,
    vehicleVariant :: DVV.VehicleVariant,
    ridesCount :: Int,
    favRiderCount :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

$(YTH.generateGenericDefault ''TagData)
$(YTH.generateGenericDefaultWithOverrides [("isDriverSameAsCustomer", ["False"])] ''EndRideTagData)
$(YTH.generateGenericDefault ''SelectTagData)
$(YTH.generateGenericDefault ''UpgradeTierTagData)

instance YTC.LogicInputLink YA.ApplicationEvent where
  getLogicInputDef a =
    case a of
      YA.Search -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @TagData)
      YA.Select -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @SelectTagData)
      YA.RideEnd -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @EndRideTagData)
      YA.UpgradeTier -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @UpgradeTierTagData)
      _ -> Nothing
