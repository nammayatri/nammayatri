module Epass.External.Geo.Types where

import           Data.Aeson
import           EulerHS.Prelude

data ReverseGeoResp =
  ReverseGeoResp
    { address_components :: [Address]
    , formated_address   :: Text
    , place_id           :: Text
    , types              :: [Text]
    , geometry           :: Value
    }
  deriving (Generic, FromJSON, ToJSON)

data Address =
  Address
    { long_name  :: Text
    , short_name :: Text
    , types      :: [Text]
    }
  deriving (Generic, FromJSON, ToJSON)
