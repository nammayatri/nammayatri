module Types.API.Vehicle where

import Beckn.Types.APISuccess
import Beckn.Types.Common as BC
import Beckn.Types.Id
import Beckn.Types.Predicate
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.Validation
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Organization as Org
import Domain.Types.Vehicle as SV
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.Prelude hiding (id)

-- Create Person request and response
data CreateVehicleReq = CreateVehicleReq
  { variant :: Variant.Variant,
    model :: Text,
    color :: Text,
    capacity :: Maybe Int,
    category :: Maybe Category,
    make :: Maybe Text,
    size :: Maybe Text,
    energyType :: Maybe EnergyType,
    registrationNo :: Text,
    registrationCategory :: Maybe RegistrationCategory
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

-- why model can't contain digits?
validateCreateVehicleReq :: Validate CreateVehicleReq
validateCreateVehicleReq CreateVehicleReq {..} =
  sequenceA_
    [ validateField "model" model $
        NotEmpty `And` star P.latinOrSpace,
      validateField "color" color $ NotEmpty `And` P.name,
      validateField "registrationNo" registrationNo $
        LengthInRange 1 11 `And` star (P.latinUC \/ P.digit),
      validateField "make" make . InMaybe $ NotEmpty `And` P.name
    ]

createVehicle :: MonadFlow m => CreateVehicleReq -> Id Org.Organization -> m SV.Vehicle
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
  deriving (Generic, ToJSON, ToSchema)

newtype ListVehicleRes = ListVehicleRes
  {vehicles :: [VehicleRes]}
  deriving (Generic, ToJSON, ToSchema)

data UpdateVehicleReq = UpdateVehicleReq
  { variant :: Maybe Variant.Variant,
    model :: Maybe Text,
    color :: Maybe Text,
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
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data Driver = Driver
  { id :: Text,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    rating :: Maybe Int,
    organizationId :: Maybe (Id Org.Organization)
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)
