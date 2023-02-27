{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Driver.DriverFlowStatus
  ( FlowStatus (..),
    DriverFlowStatus (..),
  )
where

import Data.Aeson (Options (..), SumEncoding (..), defaultOptions)
import Data.OpenApi
import qualified Domain.Types.DriverQuote as DQ
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import Kernel.Prelude
import Kernel.Types.Id

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
