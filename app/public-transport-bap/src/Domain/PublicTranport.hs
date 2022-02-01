{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.PublicTranport where

import Beckn.Prelude
import Beckn.Types.Id

data PublicTranport = PublicTranport
  { id :: Id PublicTranport,
    name :: Text,
    stationCode :: Text,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic)

data PublicTranportAPIEntity = PublicTranportAPIEntity
  { name :: Text,
    stationCode :: Text,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, ToJSON, ToSchema)

makePublicTranportAPIEntity :: PublicTranport -> PublicTranportAPIEntity
makePublicTranportAPIEntity PublicTranport {..} =
  PublicTranportAPIEntity {..}
