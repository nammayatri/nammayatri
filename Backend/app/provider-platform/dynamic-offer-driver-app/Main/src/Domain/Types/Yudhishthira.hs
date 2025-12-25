{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Yudhishthira where

import qualified Data.Aeson as A
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as DBCR
-- import qualified Domain.Types.Estimate as DEst

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.VehicleVariant as DVV
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types.Application as YA
import qualified Lib.Yudhishthira.Types.Common as YTC
import qualified Lib.Yudhishthira.TypesTH as YTH

data CancelRideTagData = CancelRideTagData
  { ride :: DRide.Ride,
    booking :: SRB.Booking,
    cancellationReason :: DBCR.BookingCancellationReason,
    callAtemptByDriver :: Bool,
    currentTime :: Int,
    rideCreatedTime :: Int,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    driverArrivalTime :: Maybe Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data TagData = TagData
  { searchRequest :: DSR.SearchRequest,
    area :: Text,
    specialLocationTag :: Maybe Text,
    specialLocationName :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data EndRideTagData = EndRideTagData
  { ride :: DRide.Ride,
    booking :: SRB.Booking
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data SelectTagData = SelectTagData
  { isPetRide :: Bool --,
  -- estimates :: [DEst.Estimate] -------uncomment this line if you want to use estimates in select tag data
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data PenaltyCheckTagData = PenaltyCheckTagData
  { ride :: DRide.Ride,
    booking :: SRB.Booking,
    currentTime :: Int,
    rideCreatedTime :: Int,
    driverArrivedAtPickup :: Bool,
    driverDistanceFromPickupNow :: Maybe Meters,
    driverDistanceFromPickupAtAcceptance :: Maybe Meters,
    numberOfCallAttempts :: Int
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
$(YTH.generateGenericDefault ''EndRideTagData)
$(YTH.generateGenericDefault ''CancelRideTagData)
$(YTH.generateGenericDefault ''SelectTagData)
$(YTH.generateGenericDefault ''PenaltyCheckTagData)
$(YTH.generateGenericDefault ''UpgradeTierTagData)

instance YTC.LogicInputLink YA.ApplicationEvent where
  getLogicInputDef a =
    case a of
      YA.Search -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @TagData)
      YA.Select -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @SelectTagData)
      YA.RideEnd -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @EndRideTagData)
      YA.RideCancel -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @CancelRideTagData)
      YA.PenaltyCheck -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @PenaltyCheckTagData)
      YA.UpgradeTier -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @UpgradeTierTagData)
      _ -> Nothing
