{-# LANGUAGE OverloadedLabels #-}

module Product.Vehicle where

import App.Types
import Beckn.TypeClass.Transform
import Beckn.Types.App
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Types.Storage.Vehicle as SV
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Vehicle as QV
import Types.API.Vehicle
import qualified Utils.Defaults as Default

createVehicle :: Text -> CreateVehicleReq -> FlowHandler CreateVehicleRes
createVehicle orgId req = withFlowHandler $ do
  vehicle <- createTransform req >>= addOrgId orgId
  QV.create vehicle
  return $ CreateVehicleRes vehicle

listVehicles :: Text -> Maybe SV.Variant -> Maybe SV.Category -> Maybe SV.EnergyType -> Maybe Int -> Maybe Int -> FlowHandler ListVehicleRes
listVehicles orgId variantM categoryM energyTypeM limitM offsetM =
  withFlowHandler $
    ListVehicleRes <$> QV.findAllByVariantCatOrgId variantM categoryM energyTypeM limit offset orgId
  where
    limit = toInteger $ fromMaybe Default.limit limitM
    offset = toInteger $ fromMaybe Default.offset offsetM

updateVehicle :: Text -> Text -> UpdateVehicleReq -> FlowHandler UpdateVehicleRes
updateVehicle orgId vehicleId req = withFlowHandler $ do
  vehicle <- QV.findByIdAndOrgId (VehicleId {_getVechicleId = vehicleId}) orgId
  updatedVehicle <- modifyTransform req vehicle
  QV.updateVehicleRec updatedVehicle
  return $ CreateVehicleRes {vehicle = updatedVehicle}

deleteVehicle :: Text -> Text -> FlowHandler DeleteVehicleRes
deleteVehicle orgId vehicleId = withFlowHandler $ do
  vehicle <-
    QV.findVehicleById (VehicleId vehicleId)
      >>= fromMaybeM400 "VEHICLE_NOT_FOUND"
  if vehicle ^. #_organizationId == orgId
    then do
      QV.deleteById (VehicleId vehicleId)
      return $ DeleteVehicleRes vehicleId
    else L.throwException $ err401 {errBody = "Unauthorized"}

getVehicle :: SR.RegistrationToken -> Maybe Text -> Maybe Text -> FlowHandler CreateVehicleRes
getVehicle SR.RegistrationToken {..} registrationNoM vehicleIdM = withFlowHandler $ do
  user <- QP.findPersonById (PersonId _EntityId)
  vehicle <- case (registrationNoM, vehicleIdM) of
    (Nothing, Nothing) -> L.throwException $ err400 {errBody = "Invalid Request"}
    _ ->
      QV.findByAnyOf registrationNoM vehicleIdM
        >>= fromMaybeM400 "VEHICLE NOT FOUND"
  hasAccess user vehicle
  return $ CreateVehicleRes vehicle
  where
    hasAccess :: SP.Person -> SV.Vehicle -> Flow ()
    hasAccess user vehicle =
      when (user ^. #_organizationId /= Just (vehicle ^. #_organizationId)) $
        L.throwException $
          err401 {errBody = "Unauthorized"}

addOrgId :: Text -> SV.Vehicle -> Flow SV.Vehicle
addOrgId orgId vehicle = return $ vehicle {SV._organizationId = orgId}
