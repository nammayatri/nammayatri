module Lib.GoogleConfig where

import Beckn.Prelude
import Beckn.Utils.Dhall (FromDhall)

data GoogleCfgUnencrypted = GoogleCfgUnencrypted
  { googleMapsUrl :: BaseUrl,
    googleRoadsUrl :: BaseUrl,
    googleKey :: Text
  }
  deriving (Generic, FromDhall)
