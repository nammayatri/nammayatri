module Beckn.Types.API.Leads where

import Beckn.Types.Core.Context
import Beckn.Types.Core.Location as CL
import Beckn.Types.Core.Tag
import Beckn.Types.Mobility.Vehicle as MV
import Data.Time
import EulerHS.Prelude

data LeadsReq = LeadsReq
  { _context :: Context,
    _message :: LeadsReqMsg
  }
  deriving (Generic)

data LeadsReqMsg = LeadsReqMsg
  { domain :: Text, -- "MOBILITY", "FINAL-MILE-DELIVERY", "FOOD-AND-BEVERAGE"
    origin :: CL.Location,
    destination :: CL.Location,
    time :: UTCTime, -- ["format" : "date-time"]
    vehicle :: MV.Vehicle,
    -- , payload :: {
    --     travellers :: {
    --       count :: Int -- [minimum" : 1]
    --       , luggage :: {
    --           count :: Int -- [minimum" : 0]
    --       }
    --     }
    -- }
    _fare_range :: FareRange,
    _tags :: [Tag]
  }
  deriving (Generic)

instance FromJSON LeadsReqMsg where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data FareRange = FareRange
  { min :: Int,
    max :: Int,
    unit :: Text
  }
  deriving (Generic)

instance FromJSON FareRange where
  parseJSON = genericParseJSON stripAllLensPrefixOptions
