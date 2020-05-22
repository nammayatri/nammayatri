{-# LANGUAGE OverloadedLabels      #-}
module Types.API.Vehicle where

import qualified EulerHS.Language as L
import Beckn.Types.Common as BC
import Beckn.Types.Storage.Vehicle as SV
import Data.Swagger
import Beckn.Utils.Extra
import Beckn.Types.App
import Beckn.Types.Common
import EulerHS.Prelude
import Data.Generics.Labels
import Servant.Swagger
import Beckn.TypeClass.Transform

-- Create Person request and response
data CreateVehicleReq = CreateVehicleReq
  { _capacity :: Maybe Int
    , _category :: Maybe Category
    , _make :: Maybe Text
    , _model :: Maybe Text
    , _size :: Maybe Text
    , _variant :: Maybe Variant
    , _color :: Maybe Text
    , _energyType :: Maybe EnergyType
    , _registrationNo :: Text
    , _registrationCategory :: Maybe RegistrationCategory
  }
  deriving (Generic, ToSchema)

instance FromJSON CreateVehicleReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance Transform2 CreateVehicleReq SV.Vehicle where
  transformFlow req = do
    id              <- BC.generateGUID
    now             <- getCurrentTimeUTC
    return $ SV.Vehicle {
      -- only these below will be updated in the person table. if you want to add something extra please add in queries also
        SV._id = id
      , SV._capacity = req ^. #_capacity
      , SV._category = req ^. #_category
      , SV._make = req ^. #_make
      , SV._model = req ^. #_model
      , SV._size = req ^. #_size
      , SV._organizationId = "WILL_BE_UPDATED_BEFORE_DB"
      , SV._variant = req ^. #_variant
      , SV._color = req ^. #_color
      , SV._energyType = req ^. #_energyType
      , SV._registrationNo = req ^. #_registrationNo
      , SV._registrationCategory = req ^. #_registrationCategory
      , SV._createdAt =  now
      , SV._updatedAt = now
    }

data CreateVehicleRes = CreateVehicleRes
  {  vehicle :: SV.Vehicle}
  deriving (Generic, ToJSON, ToSchema)

data ListVehicleRes = ListVehicleRes
  {  vehicles :: [SV.Vehicle] }
  deriving (Generic, ToJSON, ToSchema)