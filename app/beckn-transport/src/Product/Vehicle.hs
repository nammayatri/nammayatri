module Product.Vehicle where

import App.Types
import Beckn.Types.Id
import Beckn.Utils.Validation (runRequestValidation)
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Vehicle as QV
import Types.API.Vehicle as API
import Types.Error
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.Person as SP
import qualified Types.Storage.Vehicle as SV
import Utils.Common
import qualified Utils.Defaults as Default

createVehicle :: Text -> CreateVehicleReq -> FlowHandler CreateVehicleRes
createVehicle orgId req = withFlowHandlerAPI $ do
  runRequestValidation validateCreateVehicleReq req
  validateVehicle
  vehicle <- API.createVehicle req (Id orgId)
  QV.createFlow vehicle
  return $ CreateVehicleRes vehicle
  where
    validateVehicle = do
      mVehicle <- QV.findByRegistrationNo $ req.registrationNo
      when (isJust mVehicle) $
        throwError $ InvalidRequest "Registration number already exists."

listVehicles :: Text -> Maybe SV.Variant -> Maybe SV.Category -> Maybe SV.EnergyType -> Maybe Text -> Maybe Int -> Maybe Int -> FlowHandler ListVehicleRes
listVehicles orgId variantM categoryM energyTypeM mbRegNum limitM offsetM = withFlowHandlerAPI $ do
  personList <- QP.findAllByOrgIds [SP.DRIVER] [Id orgId]
  vehicleList <- QV.findAllByVariantCatOrgId variantM categoryM energyTypeM mbRegNum limit offset $ Id orgId
  let respList = mkVehicleRes personList <$> vehicleList
  return $ ListVehicleRes respList
  where
    limit = toInteger $ fromMaybe Default.limit limitM
    offset = toInteger $ fromMaybe Default.offset offsetM

updateVehicle :: Text -> Id SV.Vehicle -> UpdateVehicleReq -> FlowHandler UpdateVehicleRes
updateVehicle orgId vehicleId req = withFlowHandlerAPI $ do
  runRequestValidation validateUpdateVehicleReq req
  vehicle <- QV.findByIdAndOrgId vehicleId (Id orgId) >>= fromMaybeM VehicleDoesNotExist
  updatedVehicle <- modifyVehicle req vehicle
  QV.updateVehicleRec updatedVehicle
  return $ CreateVehicleRes {vehicle = updatedVehicle}

deleteVehicle :: Text -> Id SV.Vehicle -> FlowHandler DeleteVehicleRes
deleteVehicle orgId vehicleId = withFlowHandlerAPI $ do
  vehicle <-
    QV.findVehicleById vehicleId
      >>= fromMaybeM VehicleDoesNotExist
  if vehicle.organizationId == Id orgId
    then do
      QV.deleteById vehicleId
      return . DeleteVehicleRes $ getId vehicleId
    else throwError Unauthorized

getVehicle :: Id SP.Person -> Maybe Text -> Maybe (Id SV.Vehicle) -> FlowHandler CreateVehicleRes
getVehicle personId registrationNoM vehicleIdM = withFlowHandlerAPI $ do
  user <-
    QP.findPersonById personId
      >>= fromMaybeM PersonNotFound
  vehicle <- case (registrationNoM, vehicleIdM) of
    (Nothing, Nothing) -> throwError $ InvalidRequest "You should pass registration number and vehicle id."
    _ ->
      QV.findByAnyOf registrationNoM vehicleIdM
        >>= fromMaybeM VehicleDoesNotExist
  hasAccess user vehicle
  return $ CreateVehicleRes vehicle
  where
    hasAccess user vehicle =
      when (user.organizationId /= Just (vehicle.organizationId)) $
        throwError Unauthorized

addOrgId :: Id Org.Organization -> SV.Vehicle -> SV.Vehicle
addOrgId orgId vehicle = vehicle {SV.organizationId = orgId}

mkVehicleRes :: [SP.Person] -> SV.Vehicle -> VehicleRes
mkVehicleRes personList vehicle =
  let mdriver =
        find
          ( \person ->
              person.udf1 == Just (getId $ vehicle.id)
          )
          personList
   in VehicleRes
        { vehicle = vehicle,
          driver = mkDriverObj <$> mdriver
        }

mkDriverObj :: SP.Person -> Driver
mkDriverObj person =
  Driver
    { id = getId $ person.id,
      firstName = person.firstName,
      middleName = person.middleName,
      lastName = person.lastName,
      fullName = person.fullName,
      rating = round <$> person.rating,
      verified = person.verified,
      organizationId = person.organizationId
    }
