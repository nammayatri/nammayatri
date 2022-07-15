module Product.Vehicle where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Validation (runRequestValidation)
import qualified Domain.Types.Organization as Org
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Vehicle as SV
import qualified Domain.Types.Vehicle.Variant as Variant
import Environment
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Vehicle as QV
import Types.API.Vehicle as API
import Types.Error
import Utils.Common
import qualified Utils.Defaults as Default

listVehicles :: SP.Person -> Maybe Variant.Variant -> Maybe Text -> Maybe Int -> Maybe Int -> FlowHandler ListVehicleRes
listVehicles admin variantM mbRegNum limitM offsetM = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  personList <- QP.findAllByOrgId [SP.DRIVER] orgId
  vehicleList <- QV.findAllByVariantRegNumOrgId variantM mbRegNum limit offset orgId
  respList <- buildVehicleRes personList `traverse` vehicleList
  return $ ListVehicleRes respList
  where
    limit = toInteger $ fromMaybe Default.limit limitM
    offset = toInteger $ fromMaybe Default.offset offsetM

updateVehicle :: SP.Person -> Id SP.Person -> API.UpdateVehicleReq -> FlowHandler API.UpdateVehicleRes
updateVehicle admin driverId req = withFlowHandlerAPI $ do
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

getVehicle :: Id SP.Person -> Maybe Text -> Maybe (Id SP.Person) -> FlowHandler GetVehicleRes
getVehicle personId registrationNoM vehicleIdM = withFlowHandlerAPI $ do
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

addOrgId :: Id Org.Organization -> SV.Vehicle -> SV.Vehicle
addOrgId orgId vehicle = vehicle {SV.organizationId = orgId}

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
