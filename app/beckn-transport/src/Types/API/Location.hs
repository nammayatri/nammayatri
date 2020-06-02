{-# LANGUAGE OverloadedLabels #-}

module Types.API.Location where

import Beckn.Types.App
import Beckn.Types.Common as BC
import Beckn.Types.Common
import qualified Beckn.Types.Storage.Location as SL
import Beckn.Utils.Extra
import Data.Generics.Labels
import Data.Swagger
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant.Swagger

data UpdateLocationReq = UpdateLocationReq
  { locationType :: Maybe SL.LocationType,
    lat :: Maybe Double,
    long :: Maybe Double,
    ward :: Maybe Text,
    district :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    pincode :: Maybe Text,
    address :: Maybe Text,
    bound :: Maybe Text
  }
  deriving (Generic, ToSchema, ToJSON, Show, FromJSON)

data UpdateLocationRes = UpdateLocationRes
  { status :: Text
  }
  deriving (Generic, ToJSON, ToSchema)

data GetLocationRes = GetLocationRes
  { location :: Maybe UpdateLocationReq
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)
