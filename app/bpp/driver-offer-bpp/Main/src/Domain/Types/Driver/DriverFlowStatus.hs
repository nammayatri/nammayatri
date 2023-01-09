module Domain.Types.Driver.DriverFlowStatus
  ( FlowStatus (..),
    DriverFlowStatus (..),
  )
where

import Beckn.Prelude
import Beckn.Types.Id
import Data.Aeson (Options (..), SumEncoding (..), defaultOptions)
import Data.OpenApi
import qualified Domain.Types.DriverQuote as DQ
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR

-- Warning: This whole thing is for frontend use only, don't make any backend logic based on this.
data FlowStatus
  = IDLE
  | ACTIVE
  | GOT_SEARCH_REQUEST
      { requestId :: Id DSR.SearchRequest,
        validTill :: UTCTime
      }
  | OFFERED_QUOTE
      { quoteId :: Id DQ.DriverQuote,
        validTill :: UTCTime
      }
  | RIDE_ASSIGNED
      { rideId :: Id DRide.Ride
      }
  | WAITING_FOR_CUSTOMER
      { rideId :: Id DRide.Ride
      }
  | ON_RIDE
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

data DriverFlowStatus = DriverFlowStatus
  { personId :: Id DP.Person,
    flowStatus :: FlowStatus,
    updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
