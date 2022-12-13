{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.Common.Ride
  ( module Dashboard.Common.Ride,
    module Reexport,
  )
where

import Beckn.External.Maps.Types
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Common
import Beckn.Types.Id
import Dashboard.Common as Reexport
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Servant

---------------------------------------------------------
-- ride list --------------------------------------------

type RideListAPI =
  "list"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "bookingStatus" BookingStatus
    :> QueryParam "rideId" (Id Ride)
    :> QueryParam "customerPhoneNo" Text
    :> QueryParam "driverPhoneNo" Text
    :> Get '[JSON] RideListRes

data RideListRes = RideListRes
  { totalItems :: Int,
    rides :: [RideListItem]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideListItem = RideListItem
  { rideId :: Id Ride,
    driverId :: Id Driver,
    customerName :: Maybe Text,
    customerPhoneNo :: Text,
    driverName :: Text,
    driverPhoneNo :: Maybe Text,
    vehicleNo :: Maybe Text, -- may be Nothing, when we unlink vehicle
    fromLocationArea :: Maybe Text,
    toLocationArea :: Maybe Text,
    bookingStatus :: BookingStatus
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BookingStatus = UPCOMING | ONGOING | ONGOING_6HRS | COMPLETED | CANCELLED
  deriving stock (Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

derivePersistField "BookingStatus"

-- TODO move similar instances to Lib
instance FromHttpApiData BookingStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData BookingStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

---------------------------------------------------------
-- ride start -------------------------------------------

type RideStartAPI =
  Capture "rideId" (Id Ride)
    :> "start"
    :> ReqBody '[JSON] StartRideReq
    :> Post '[JSON] APISuccess

newtype StartRideReq = StartRideReq
  { point :: Maybe LatLong
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- ride end ---------------------------------------------

type RideEndAPI =
  Capture "rideId" (Id Ride)
    :> "end"
    :> ReqBody '[JSON] EndRideReq
    :> Post '[JSON] APISuccess

newtype EndRideReq = EndRideReq
  { point :: Maybe LatLong
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- ride cancel ------------------------------------------

type RideCancelAPI =
  Capture "rideId" (Id Ride)
    :> "cancel"
    :> ReqBody '[JSON] CancelRideReq
    :> Post '[JSON] APISuccess

data CancelRideReq = CancelRideReq
  { reasonCode :: CancellationReasonCode,
    additionalInfo :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype CancellationReasonCode = CancellationReasonCode Text
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- ride info ------------------------------------------

type RideInfoAPI =
  Capture "rideId" (Id Ride)
    :> "info"
    :> Get '[JSON] RideInfoRes

data RideInfoRes = RideInfoRes
  { rideId :: Id Ride,
    customerName :: Maybe Text,
    customerPhoneNo :: Text,
    rideOtp :: Text,
    customerPickupLocation :: LocationAPIEntity,
    customerDropLocation :: Maybe LocationAPIEntity,
    driverId :: Id Driver,
    driverName :: Text,
    driverPhoneNo :: Maybe Text,
    vehicleNo :: Maybe Text,
    driverStartLocation :: Maybe LatLong,
    driverCurrentLocation :: LatLong,
    rideBookingTime :: UTCTime,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    rideDistanceEstimated :: Maybe Meters,
    rideDistanceActual :: Meters,
    pickupDuration :: Maybe Minutes,
    rideDuration :: Maybe Minutes,
    bookingStatus :: BookingStatus,
    cancelledBy :: Maybe CancellationSource,
    cancellationReason :: Maybe CancellationReasonCode,
    driverInitiatedCallCount :: Int,
    bookingToRideStartDuration :: Maybe Minutes
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LocationAPIEntity = LocationAPIEntity
  { lat :: Double,
    lon :: Double,
    street :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CancellationSource
  = ByUser
  | ByDriver
  | ByMerchant
  | ByAllocator
  | ByApplication
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
