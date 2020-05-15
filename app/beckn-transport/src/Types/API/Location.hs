{-# LANGUAGE OverloadedLabels      #-}
module Types.API.Location where

import qualified Beckn.Types.Storage.Location as SL
import qualified EulerHS.Language as L
import Beckn.Types.Common as BC
import Data.Swagger
import Beckn.Utils.Extra
import Beckn.Types.App
import Beckn.Types.Common
import EulerHS.Prelude
import Data.Generics.Labels
import Servant.Swagger

data UpdateLocationReq = UpdateLocationReq
  { locationType :: Maybe SL.LocationType
  , lat :: Maybe Double
  , long :: Maybe Double
  , ward :: Maybe Text
  , district :: Maybe Text
  , city :: Maybe Text
  , state :: Maybe Text
  , country :: Maybe Text
  , pincode :: Maybe Text
  , address :: Maybe Text
  , bound :: Maybe Text
  }
  deriving (Generic, ToSchema,ToJSON, Show, FromJSON)

data UpdateLocationRes = UpdateLocationRes
  {
    status :: Text
  }
  deriving (Generic, ToJSON, ToSchema)

data GetLocationRes = GetLocationRes
  {
    location :: Maybe UpdateLocationReq
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)