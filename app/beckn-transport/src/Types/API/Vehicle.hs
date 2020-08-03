{-# LANGUAGE OverloadedLabels #-}

module Types.API.Vehicle where

import App.Types
import Beckn.TypeClass.Transform
import Beckn.Types.Common as BC
import Beckn.Types.Storage.Vehicle as SV
import Beckn.Utils.Extra
import Data.Swagger
import EulerHS.Prelude

-- Create Person request and response
data CreateVehicleReq = CreateVehicleReq
  { _capacity :: Maybe Int,
    _category :: Maybe Category,
    _make :: Maybe Text,
    _model :: Maybe Text,
    _size :: Maybe Text,
    _variant :: Maybe Variant,
    _color :: Maybe Text,
    _energyType :: Maybe EnergyType,
    _registrationNo :: Text,
    _registrationCategory :: Maybe RegistrationCategory
  }
  deriving (Generic, ToSchema)

instance FromJSON CreateVehicleReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance CreateTransform CreateVehicleReq SV.Vehicle Flow where
  createTransform req = do
    vid <- BC.generateGUID
    now <- getCurrentTimeUTC
    return $
      SV.Vehicle
        { -- only these below will be updated in the vehicle table. if you want to add something extra please add in queries also
          SV._id = vid,
          SV._capacity = req ^. #_capacity,
          SV._category = req ^. #_category,
          SV._make = req ^. #_make,
          SV._model = req ^. #_model,
          SV._size = req ^. #_size,
          SV._organizationId = "WILL_BE_UPDATED_BEFORE_DB",
          SV._variant = req ^. #_variant,
          SV._color = req ^. #_color,
          SV._energyType = req ^. #_energyType,
          SV._registrationNo = req ^. #_registrationNo,
          SV._registrationCategory = req ^. #_registrationCategory,
          SV._createdAt = now,
          SV._updatedAt = now
        }

newtype CreateVehicleRes = CreateVehicleRes
  {vehicle :: SV.Vehicle}
  deriving (Generic, ToJSON, ToSchema)

newtype ListVehicleRes = ListVehicleRes
  {vehicles :: [SV.Vehicle]}
  deriving (Generic, ToJSON, ToSchema)

data UpdateVehicleReq = UpdateVehicleReq
  { _capacity :: Maybe Int,
    _category :: Maybe Category,
    _make :: Maybe Text,
    _model :: Maybe Text,
    _size :: Maybe Text,
    _variant :: Maybe Variant,
    _color :: Maybe Text,
    _energyType :: Maybe EnergyType,
    _registrationCategory :: Maybe RegistrationCategory
  }
  deriving (Generic, ToSchema)

instance FromJSON UpdateVehicleReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ModifyTransform UpdateVehicleReq SV.Vehicle Flow where
  modifyTransform req vehicle = do
    now <- getCurrentTimeUTC
    return $
      vehicle
        { -- only these below will be updated in the vehicle table. if you want to add something extra please add in queries also
          SV._capacity = (req ^. #_capacity) <|> (vehicle ^. #_capacity),
          SV._category = (req ^. #_category) <|> (vehicle ^. #_category),
          SV._make = (req ^. #_make) <|> (vehicle ^. #_make),
          SV._model = (req ^. #_model) <|> (vehicle ^. #_model),
          SV._size = (req ^. #_size) <|> (vehicle ^. #_size),
          SV._variant = (req ^. #_variant) <|> (vehicle ^. #_variant),
          SV._color = (req ^. #_color) <|> (vehicle ^. #_color),
          SV._energyType = (req ^. #_energyType) <|> (vehicle ^. #_energyType),
          SV._registrationCategory = (req ^. #_registrationCategory) <|> (vehicle ^. #_registrationCategory),
          SV._updatedAt = now
        }

type UpdateVehicleRes = CreateVehicleRes

newtype DeleteVehicleRes = DeleteVehicleRes
  {vehicleId :: Text}
  deriving (Generic, ToJSON, ToSchema)
