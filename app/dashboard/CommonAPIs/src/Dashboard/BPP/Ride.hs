{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.BPP.Ride
  ( module Dashboard.BPP.Ride,
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
import Servant hiding (Summary)

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data RideEndpoint
  = RideStartEndpoint
  | RideEndEndpoint
  | RideCancelEndpoint
  | RideSyncEndpoint
  deriving (Show, Read)

derivePersistField "RideEndpoint"

---------------------------------------------------------
-- ride list --------------------------------------------

type RideListAPI =
  "list"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "bookingStatus" BookingStatus
    :> QueryParam "rideShortId" (ShortId Ride)
    :> QueryParam "customerPhoneNo" Text
    :> QueryParam "driverPhoneNo" Text
    :> Get '[JSON] RideListRes

data RideListRes = RideListRes
  { totalItems :: Int, -- for backward compatibility
    summary :: Summary,
    rides :: [RideListItem]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideListItem = RideListItem
  { rideId :: Id Ride,
    rideShortId :: ShortId Ride,
    customerName :: Maybe Text,
    customerPhoneNo :: Text,
    driverName :: Text,
    driverPhoneNo :: Maybe Text,
    vehicleNo :: Text,
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

instance HideSecrets StartRideReq where
  hideSecrets = identity

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

instance HideSecrets EndRideReq where
  hideSecrets = identity

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

instance HideSecrets CancelRideReq where
  hideSecrets = identity

---------------------------------------------------------
-- ride info --------------------------------------------

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
    actualDropLocation :: Maybe LatLong,
    driverId :: Id Driver,
    driverName :: Text,
    driverPhoneNo :: Maybe Text,
    vehicleNo :: Text,
    driverStartLocation :: Maybe LatLong,
    driverCurrentLocation :: LatLong,
    rideBookingTime :: UTCTime,
    estimatedDriverArrivalTime :: UTCTime,
    actualDriverArrivalTime :: Maybe UTCTime,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    rideDistanceEstimated :: Maybe Meters,
    rideDistanceActual :: Meters,
    estimatedRideDuration :: Maybe Minutes,
    estimatedFare :: Money,
    actualFare :: Maybe Money,
    pickupDuration :: Maybe Minutes,
    rideDuration :: Maybe Minutes,
    bookingStatus :: BookingStatus,
    cancelledTime :: Maybe UTCTime,
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

---------------------------------------------------------
-- ride sync ---------------------------------------------

type RideSyncAPI =
  Capture "rideId" (Id Ride)
    :> "sync"
    :> Post '[JSON] RideSyncRes

newtype RideSyncRes = RideSyncRes
  { newStatus :: RideStatus
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets RideSyncRes where
  hideSecrets = identity

data RideStatus
  = RIDE_NEW
  | RIDE_INPROGRESS
  | RIDE_COMPLETED
  | RIDE_CANCELLED
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
