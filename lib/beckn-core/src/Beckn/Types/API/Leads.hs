module Beckn.Types.API.Leads where

import           EulerHS.Prelude
import           Data.Time
import           Beckn.Types.Core.Context
import           Beckn.Types.Mobility.Vehicle as MV
import           Beckn.Types.Core.Location as CL
import           Beckn.Types.Core.Tag

data LeadsReq =
  LeadsReq
    { _context :: Context
    , _message :: LeadsReqMsg
    }
      deriving (Generic)

data LeadsReqMsg = 
  LeadsReqMsg
    { domain :: Text  -- "MOBILITY", "FINAL-MILE-DELIVERY", "FOOD-AND-BEVERAGE"
    , origin :: CL.Location
    , destination :: CL.Location
    , time :: LocalTime -- ["format" : "date-time"]
    , vehicle :: MV.Vehicle
    -- , payload :: { 
    --     travellers :: {
    --       count :: Int -- [minimum" : 1]
    --       , luggage :: {
    --           count :: Int -- [minimum" : 0]
    --       }
    --     }
    -- }
    , _fare_range :: FareRange
    , _tags :: [Tag]
    }
      deriving (Generic)

instance FromJSON LeadsReqMsg where
  parseJSON = genericParseJSON stripAllLensPrefixOptions


data FareRange =
  FareRange
    { min :: Int
    , max :: Int
    , unit :: Text
    } 
      deriving (Generic)

instance FromJSON FareRange where
  parseJSON = genericParseJSON stripAllLensPrefixOptions