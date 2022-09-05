module Domain.Action.UI.Vehicle
  ( UpdateVehicleReq (..),
    GetVehicleRes (..),
    ListVehicleRes (..),
    UpdateVehicleRes,
    VehicleRes (..),
    Driver (..),
    listVehicles,
    updateVehicle,
    getVehicle,
  )
where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Types.Predicate
import Beckn.Utils.Common
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.Validation
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Organization as Org
import qualified Domain.Types.Person as SP
import Domain.Types.Vehicle as SV
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Vehicle as QV
import Types.Error

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

listVehicles :: EsqDBFlow m r => SP.Person -> Maybe Variant.Variant -> Maybe Text -> Maybe Int -> Maybe Int -> m ListVehicleRes
listVehicles admin variantM mbRegNum limitM offsetM = do
  let Just orgId = admin.organizationId
  personList <- QP.findAllByOrgId [SP.DRIVER] orgId
  vehicleList <- QV.findAllByVariantRegNumOrgId variantM mbRegNum limit offset orgId
  respList <- buildVehicleRes personList `traverse` vehicleList
  return $ ListVehicleRes respList
  where
    limit = toInteger $ fromMaybe 50 limitM
    offset = toInteger $ fromMaybe 0 offsetM

updateVehicle :: EsqDBFlow m r => SP.Person -> Id SP.Person -> UpdateVehicleReq -> m UpdateVehicleRes
updateVehicle admin driverId req = do
  let Just orgId = admin.organizationId
  runRequestValidation validateUpdateVehicleReq req
  driver <- QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  unless (driver.organizationId == Just orgId || driver.role == SP.DRIVER) $ throwError Unauthorized
  vehicle <- QV.findById driverId >>= fromMaybeM (VehicleNotFound driverId.getId)
  whenJust req.registrationNo $ \regNum -> do
    vehicleWithRegistrationNoM <- QV.findByRegistrationNo regNum
    when (isJust vehicleWithRegistrationNoM) $
      throwError $ InvalidRequest "Registration number already exists."
  let updatedVehicle =
        vehicle{variant = fromMaybe vehicle.variant req.variant,
                model = fromMaybe vehicle.model req.model,
                color = fromMaybe vehicle.color req.color,
                capacity = req.capacity <|> vehicle.capacity,
                category = req.category <|> vehicle.category,
                make = req.make <|> vehicle.make,
                size = req.size <|> vehicle.size,
                energyType = req.energyType <|> vehicle.energyType,
                registrationNo = fromMaybe vehicle.registrationNo req.registrationNo,
                registrationCategory = req.registrationCategory <|> vehicle.registrationCategory
               }

  Esq.runTransaction $ QV.updateVehicleRec updatedVehicle
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> updateVehicle : ") (show updatedVehicle)
  return $ SV.makeVehicleAPIEntity updatedVehicle

getVehicle :: EsqDBFlow m r => Id SP.Person -> Maybe Text -> Maybe (Id SP.Person) -> m GetVehicleRes
getVehicle personId registrationNoM vehicleIdM = do
  user <-
    QP.findById personId
      >>= fromMaybeM (PersonNotFound personId.getId)
  vehicle <- case (registrationNoM, vehicleIdM) of
    (Nothing, Nothing) -> throwError $ InvalidRequest "You should pass registration number and vehicle id."
    _ ->
      QV.findByAnyOf registrationNoM vehicleIdM
        >>= fromMaybeM (VehicleDoesNotExist personId.getId)
  hasAccess user vehicle
  return . GetVehicleRes $ SV.makeVehicleAPIEntity vehicle
  where
    hasAccess user vehicle =
      when (user.organizationId /= Just (vehicle.organizationId)) $
        throwError Unauthorized

buildVehicleRes :: MonadFlow m => [SP.Person] -> SV.Vehicle -> m VehicleRes
buildVehicleRes personList vehicle = do
  let mdriver =
        find
          ( \person ->
              person.id == vehicle.driverId
          )
          personList
  return
    VehicleRes
      { vehicle = SV.makeVehicleAPIEntity vehicle,
        driver = mkDriverObj <$> mdriver
      }

mkDriverObj :: SP.Person -> Driver
mkDriverObj person =
  Driver
    { id = getId $ person.id,
      firstName = person.firstName,
      middleName = person.middleName,
      lastName = person.lastName,
      rating = round <$> person.rating,
      organizationId = person.organizationId
    }
