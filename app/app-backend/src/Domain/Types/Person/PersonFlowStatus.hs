module Domain.Types.Person.PersonFlowStatus
  ( FlowStatus (..),
    PersonFlowStatus (..),
  )
where

import Beckn.Prelude
import Beckn.Types.Id
import Data.Aeson (Options (..), SumEncoding (..), defaultOptions)
import Data.OpenApi
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.Estimate as DE

-- Warning: This whole thing is for frontend use only, don't make any backend logic based on this.
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
  | RIDE_ASSIGNED
      { rideId :: Id DRide.Ride
      }
  | PENDING_RATING
      { rideId :: Id DRide.Ride
      }
  deriving (Show, Eq, Generic)

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
