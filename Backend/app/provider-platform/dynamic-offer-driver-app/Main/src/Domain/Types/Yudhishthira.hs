{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Yudhishthira where

import qualified Data.Aeson as A
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import Kernel.Prelude
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

$(YTH.generateGenericDefault ''TagData)
$(YTH.generateGenericDefault ''EndRideTagData)
$(YTH.generateGenericDefault ''CancelRideTagData)

instance YTC.LogicInputLink YA.ApplicationEvent where
  getLogicInputDef a =
    case a of
      YA.Search -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @TagData)
      YA.RideEnd -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @EndRideTagData)
      YA.RideCancel -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @CancelRideTagData)
      _ -> Nothing
