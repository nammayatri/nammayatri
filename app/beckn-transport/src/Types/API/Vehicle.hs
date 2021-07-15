module Types.API.Vehicle where

import Beckn.Types.Common as BC
import Beckn.Types.Id
import Beckn.Types.Predicate
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Types.Storage.Vehicle as SV
import Beckn.Utils.JSON
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.Validation
import Data.Swagger
import EulerHS.Prelude hiding (id)

-- Create Person request and response
data CreateVehicleReq = CreateVehicleReq
  { capacity :: Maybe Int,
    category :: Maybe Category,
    make :: Maybe Text,
    model :: Maybe Text,
    size :: Maybe Text,
    variant :: Maybe Variant,
    color :: Maybe Text,
    energyType :: Maybe EnergyType,
    registrationNo :: Text,
    registrationCategory :: Maybe RegistrationCategory
  }
  deriving (Generic, ToSchema, FromJSON, ToJSON)

validateCreateVehicleReq :: Validate CreateVehicleReq
validateCreateVehicleReq CreateVehicleReq {..} =
  sequenceA_
    [ validateField "registrationNo" registrationNo $
        LengthInRange 1 11 `And` star (P.latinUC \/ P.digit),
      validateField "model" model . InMaybe $
        NotEmpty `And` star P.latinOrSpace,
      validateField "make" make . InMaybe $ NotEmpty `And` P.name,
      validateField "color" color . InMaybe $ NotEmpty `And` P.name
    ]

createVehicle :: DBFlow m r => CreateVehicleReq -> Id Org.Organization -> m SV.Vehicle
createVehicle req orgId = do
  vid <- BC.generateGUID
  now <- getCurrentTime
  return $
    SV.Vehicle
      { -- only these below will be updated in the vehicle table. if you want to add something extra please add in queries also
        SV.id = vid,
        SV.capacity = req.capacity,
        SV.category = req.category,
        SV.make = req.make,
        SV.model = req.model,
        SV.size = req.size,
        SV.organizationId = orgId,
        SV.variant = req.variant,
        SV.color = req.color,
        SV.energyType = req.energyType,
        SV.registrationNo = req.registrationNo,
        SV.registrationCategory = req.registrationCategory,
        SV.createdAt = now,
        SV.updatedAt = now
      }

newtype CreateVehicleRes = CreateVehicleRes
  {vehicle :: SV.Vehicle}
  deriving (Generic, ToJSON, ToSchema)

newtype ListVehicleRes = ListVehicleRes
  {vehicles :: [VehicleRes]}
  deriving (Generic, ToJSON, ToSchema)

data UpdateVehicleReq = UpdateVehicleReq
  { capacity :: Maybe Int,
    category :: Maybe Category,
    make :: Maybe Text,
    model :: Maybe Text,
    size :: Maybe Text,
    variant :: Maybe Variant,
    color :: Maybe Text,
    energyType :: Maybe EnergyType,
    registrationCategory :: Maybe RegistrationCategory
  }
  deriving (Generic, ToSchema)

instance FromJSON UpdateVehicleReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

validateUpdateVehicleReq :: Validate UpdateVehicleReq
validateUpdateVehicleReq UpdateVehicleReq {..} =
  sequenceA_
    [ validateField "model" model . InMaybe $
        NotEmpty `And` star P.latinOrSpace,
      validateField "make" make . InMaybe $ NotEmpty `And` P.name,
      validateField "color" color . InMaybe $ NotEmpty `And` P.name
    ]

modifyVehicle :: DBFlow m r => UpdateVehicleReq -> SV.Vehicle -> m SV.Vehicle
modifyVehicle req vehicle = do
  now <- getCurrentTime
  return $
    vehicle
      { -- only these below will be updated in the vehicle table. if you want to add something extra please add in queries also
        SV.capacity = (req.capacity) <|> (vehicle.capacity),
        SV.category = (req.category) <|> (vehicle.category),
        SV.make = (req.make) <|> (vehicle.make),
        SV.model = (req.model) <|> (vehicle.model),
        SV.size = (req.size) <|> (vehicle.size),
        SV.variant = (req.variant) <|> (vehicle.variant),
        SV.color = (req.color) <|> (vehicle.color),
        SV.energyType = (req.energyType) <|> (vehicle.energyType),
        SV.registrationCategory = (req.registrationCategory) <|> (vehicle.registrationCategory),
        SV.updatedAt = now
      }

type UpdateVehicleRes = CreateVehicleRes

newtype DeleteVehicleRes = DeleteVehicleRes
  {vehicleId :: Text}
  deriving (Generic, ToJSON, ToSchema)

data VehicleRes = VehicleRes
  { vehicle :: SV.Vehicle,
    driver :: Maybe Driver
  }
  deriving (Generic, ToSchema)

instance FromJSON VehicleRes where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON VehicleRes where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data Driver = Driver
  { id :: Text,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    fullName :: Maybe Text,
    rating :: Maybe Text,
    verified :: Bool,
    organizationId :: Maybe (Id Org.Organization)
  }
  deriving (Generic, ToSchema)

instance FromJSON Driver where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Driver where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
