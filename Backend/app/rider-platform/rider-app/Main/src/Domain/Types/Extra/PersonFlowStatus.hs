{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Domain.Types.Extra.PersonFlowStatus where

import qualified API.Types.UI.MultimodalConfirm as APITypes
import Data.Aeson
import Data.OpenApi
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Booking.API as SRB
import Domain.Types.Common
import qualified Domain.Types.Estimate as DE
import qualified Domain.Types.FarePolicy.FareProductType as DFPT
import qualified Domain.Types.Journey as DJ
import Kernel.Prelude
import Kernel.Types.Id
import Tools.Beam.UtilsTH (mkBeamInstancesForJSON)

-- Extra code goes here --
data FlowStatus
  = IDLE
  | WAITING_FOR_DRIVER_OFFERS
      { estimateId :: Id DE.Estimate,
        otherSelectedEstimates :: Maybe [Id DE.Estimate],
        validTill :: UTCTime,
        providerId :: Maybe Text,
        tripCategory :: Maybe TripCategory
      }
  | WAITING_FOR_DRIVER_ASSIGNMENT
      { bookingId :: Id DB.Booking,
        validTill :: UTCTime,
        fareProductType :: Maybe DFPT.FareProductType, -- TODO :: For backward compatibility, please do not maintain this in future. `fareProductType` is replaced with `tripCategory`.
        tripCategory :: Maybe TripCategory
      }
  | ACTIVE_BOOKINGS
      { list :: [SRB.BookingAPIEntity]
      }
  | ACTIVE_JOURNEYS
      { journeys :: [DJ.Journey],
        currentJourney :: Maybe APITypes.JourneyInfoResp
      }
  | FEEDBACK_PENDING SRB.BookingAPIEntity
  deriving (Show, Generic)

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
