{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Domain.Types.Extra.PersonFlowStatus where

import Data.Aeson
import Data.OpenApi
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Estimate as DE
import qualified Domain.Types.FarePolicy.FareProductType as DFPT
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Types.Id
import Tools.Beam.UtilsTH (mkBeamInstancesForJSON)

-- Extra code goes here --
data FlowStatus
  = IDLE
  | SEARCHING
      { requestId :: Id DSR.SearchRequest,
        validTill :: UTCTime
      }
  | GOT_ESTIMATE
      { requestId :: Id DSR.SearchRequest,
        validTill :: UTCTime
      }
  | WAITING_FOR_DRIVER_OFFERS
      { estimateId :: Id DE.Estimate,
        otherSelectedEstimates :: Maybe [Id DE.Estimate],
        validTill :: UTCTime
      }
  | DRIVER_OFFERED_QUOTE
      { estimateId :: Id DE.Estimate,
        validTill :: UTCTime
      }
  | WAITING_FOR_DRIVER_ASSIGNMENT
      { bookingId :: Id DB.Booking,
        validTill :: UTCTime,
        fareProductType :: Maybe DFPT.FareProductType
      }
  | RIDE_ASSIGNED -- deprecated status, kept it for backward compatibility
      { rideId :: Id DRide.Ride
      }
  | RIDE_PICKUP
      { rideId :: Id DRide.Ride,
        bookingId :: Id DB.Booking,
        trackingUrl :: Maybe BaseUrl,
        otp :: Text,
        vehicleNumber :: Text,
        fromLocation :: Maps.LatLong,
        driverLocation :: Maybe Maps.LatLong
      }
  | RIDE_STARTED
      { rideId :: Id DRide.Ride,
        bookingId :: Id DB.Booking,
        trackingUrl :: Maybe BaseUrl,
        driverLocation :: Maybe Maps.LatLong
      }
  | PENDING_RATING
      { rideId :: Id DRide.Ride,
        mbBookingId :: Maybe (Id DB.Booking)
      }
  | DRIVER_ARRIVED
      { rideId :: Id DRide.Ride,
        bookingId :: Id DB.Booking,
        driverLocation :: Maybe Maps.LatLong,
        trackingUrl :: Maybe BaseUrl,
        driverArrivalTime :: Maybe UTCTime
      }
  deriving (Show, Eq, Ord, Generic)

$(mkBeamInstancesForJSON ''FlowStatus)

flowStatusCustomJSONOptions :: Options
flowStatusCustomJSONOptions =
  defaultOptions
    { sumEncoding =
        TaggedObject
          { tagFieldName = "status",
            contentsFieldName = "info"
          }
    }

instance ToJSON FlowStatus where
  toJSON = genericToJSON flowStatusCustomJSONOptions

instance FromJSON FlowStatus where
  parseJSON = genericParseJSON flowStatusCustomJSONOptions

instance ToSchema FlowStatus where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions flowStatusCustomJSONOptions
