module Types.API.Vehicle where

import Beckn.Types.APISuccess
import Beckn.Types.Common as BC
import Beckn.Types.Id
import Beckn.Types.Predicate
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.Validation
import Data.Swagger
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Organization as Org
import Types.Storage.Vehicle as SV

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
  {vehicle :: SV.VehicleAPIEntity}
  deriving (Generic, ToJSON)

newtype ListVehicleRes = ListVehicleRes
  {vehicles :: [VehicleRes]}
  deriving (Generic, ToJSON)

data UpdateVehicleReq = UpdateVehicleReq
  { model :: Maybe Text,
    color :: Maybe Text,
    variant :: Maybe Variant,
    category :: Maybe Category
  }
  deriving (Generic, FromJSON, ToSchema)

validateUpdateVehicleReq :: Validate UpdateVehicleReq
validateUpdateVehicleReq UpdateVehicleReq {..} =
  sequenceA_
    [ validateField "model" model . InMaybe $
        NotEmpty `And` star P.latinOrSpace,
      validateField "color" color . InMaybe $ NotEmpty `And` P.name
    ]

type UpdateVehicleRes = VehicleAPIEntity

type DeleteVehicleRes = APISuccess

data VehicleRes = VehicleRes
  { vehicle :: SV.VehicleAPIEntity,
    driver :: Maybe Driver
  }
  deriving (Generic, FromJSON, ToJSON)

data Driver = Driver
  { id :: Text,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    fullName :: Maybe Text,
    rating :: Maybe Int,
    organizationId :: Maybe (Id Org.Organization)
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)
