{-# LANGUAGE OverloadedLabels #-}

module Types.API.Location where

import Beckn.Types.App
import Beckn.Types.Common as BC
import Beckn.Types.Common
import Beckn.Types.MapSearch
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
    address :: Maybe Text
  }
  deriving (Generic, ToSchema, ToJSON, Show, FromJSON)

data UpdateLocationRes = UpdateLocationRes
  { status :: Text
  }
  deriving (Generic, ToJSON, ToSchema)

data LocationInfo = LocationInfo
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
    duration :: Maybe Integer,
    distance :: Maybe Float,
    bbox :: Maybe BoundingBoxWithoutCRS,
    waypoints :: Maybe GeospatialGeometry,
    snapped_waypoints :: Maybe GeospatialGeometry
  }
  deriving (Generic, ToJSON, Show, FromJSON)

data GetLocationRes = GetLocationRes
  { location :: Maybe LocationInfo
  }
  deriving (Generic, ToJSON, FromJSON)
