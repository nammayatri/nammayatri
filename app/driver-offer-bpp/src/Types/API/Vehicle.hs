module Types.API.Vehicle where

import Beckn.Types.Id
import Beckn.Types.Predicate
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.Validation
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Organization as Org
import Domain.Types.Vehicle as SV
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.Prelude hiding (id)

data UpdateVehicleReq = UpdateVehicleReq
  { variant :: Maybe Variant.Variant,
    model :: Maybe Text,
    color :: Maybe Text,
    capacity :: Maybe Int,
    category :: Maybe Category,
    make :: Maybe Text,
    size :: Maybe Text,
    energyType :: Maybe EnergyType,
    registrationNo :: Maybe Text,
    registrationCategory :: Maybe RegistrationCategory
  }
  deriving (Generic, FromJSON, ToSchema)

newtype GetVehicleRes = GetVehicleRes
  {vehicle :: SV.VehicleAPIEntity}
  deriving (Generic, ToJSON, ToSchema)

newtype ListVehicleRes = ListVehicleRes
  {vehicles :: [VehicleRes]}
  deriving (Generic, ToJSON, ToSchema)

validateUpdateVehicleReq :: Validate UpdateVehicleReq
validateUpdateVehicleReq UpdateVehicleReq {..} =
  sequenceA_
    [ validateField "model" model . InMaybe $
        NotEmpty `And` star P.latinOrSpace,
      validateField "color" color . InMaybe $ NotEmpty `And` P.name,
      validateField "registrationNo" registrationNo . InMaybe $
        LengthInRange 1 11 `And` star (P.latinUC \/ P.digit)
    ]

type UpdateVehicleRes = VehicleAPIEntity

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
