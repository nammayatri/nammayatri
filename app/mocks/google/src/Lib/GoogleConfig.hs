module Lib.GoogleConfig where

import Kernel.Prelude
import Kernel.Utils.Dhall (FromDhall)

data GoogleCfgUnencrypted = GoogleCfgUnencrypted
  { googleMapsUrl :: BaseUrl,
    googleRoadsUrl :: BaseUrl,
    googleKey :: Text
  }
  deriving (Generic, FromDhall)
