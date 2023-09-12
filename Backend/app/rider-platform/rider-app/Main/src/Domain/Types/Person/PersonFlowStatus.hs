{-# LANGUAGE DerivingStrategies #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.Person.PersonFlowStatus
  ( FlowStatus (..),
    PersonFlowStatus (..),
  )
where

import Data.Aeson (Options (..), SumEncoding (..), defaultOptions)
import Data.OpenApi
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Estimate as DE
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Types.Id
import Tools.Beam.UtilsTH (mkBeamInstancesForJSON)

-- Warning: This whole thing is for frontend use only, don't make any backend logic based on this.
-- deriving stock instance Ord Maps.LatLong

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
        validTill :: UTCTime
      }
  | DRIVER_OFFERED_QUOTE
      { estimateId :: Id DE.Estimate,
        validTill :: UTCTime
      }
  | WAITING_FOR_DRIVER_ASSIGNMENT
      { bookingId :: Id DB.Booking,
        validTill :: UTCTime
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
      { rideId :: Id DRide.Ride
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

data PersonFlowStatus = PersonFlowStatus
  { personId :: Id DP.Person,
    flowStatus :: FlowStatus,
    updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
