{-# LANGUAGE OverloadedLabels #-}

module Product.Vehicle where

import Beckn.TypeClass.Transform
import Beckn.Types.App
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Types.Storage.Vehicle as SV
import Beckn.Utils.Common
import Data.Generics.Labels
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Vehicle as QV
import Types.API.Vehicle

createVehicle :: RegToken -> CreateVehicleReq -> FlowHandler CreateVehicleRes
createVehicle regToken req = withFlowHandler $ do
  orgId <- validate regToken
  vehicle <- transformFlow req >>= addOrgId orgId
  QV.create vehicle
  return $ CreateVehicleRes vehicle

listVehicles :: RegToken -> Maybe Integer -> Maybe Integer -> FlowHandler ListVehicleRes
listVehicles regToken limitM offsetM = withFlowHandler $ do
  orgId <- validate regToken
  ListVehicleRes <$> (QV.findAllWithLimitOffsetByOrgIds limitM offsetM [orgId])

updateVehicle :: Text -> RegToken -> UpdateVehicleReq -> FlowHandler UpdateVehicleRes
updateVehicle vehicleId regToken req = withFlowHandler $ do
  orgId <- validate regToken
  vehicle <- QV.findByIdAndOrgId (VehicleId {_getVechicleId = vehicleId}) orgId
  updatedVehicle <- transformFlow2 req vehicle
  QV.updateVehicleRec updatedVehicle
  return $ CreateVehicleRes {vehicle = updatedVehicle}

deleteVehicle :: Text -> RegToken -> FlowHandler DeleteVehicleRes
deleteVehicle vehicleId regToken = withFlowHandler $ do
  orgId <- validate regToken
  vehicle <-
    QV.findVehicleById (VehicleId vehicleId)
      >>= fromMaybeM400 "VEHICLE_NOT_FOUND"
  if vehicle ^. #_organizationId == orgId
    then do
      QV.deleteById (VehicleId vehicleId)
      return $ DeleteVehicleRes vehicleId
    else L.throwException $ err401 {errBody = "Unauthorized"}

getVehicle :: RegToken -> Maybe Text -> Maybe Text -> FlowHandler CreateVehicleRes
getVehicle token registrationNoM vehicleIdM = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyToken token
  user <- QP.findPersonById (PersonId _EntityId)
  vehicle <- case (registrationNoM, vehicleIdM) of
    (Nothing, Nothing) -> L.throwException $ err400 {errBody = "Invalid Request"}
    _ ->
      QV.findByAnyOf registrationNoM vehicleIdM
        >>= fromMaybeM400 "VEHICLE NOT FOUND"
  hasAccess user vehicle
  return $ CreateVehicleRes vehicle
  where
    hasAccess user vehicle =
      whenM (return $ (user ^. #_organizationId) /= Just (vehicle ^. #_organizationId))
        $ L.throwException
        $ err401 {errBody = "Unauthorized"}

-- Core Utility methods are below
verifyUser :: SP.Person -> L.Flow Text
verifyUser user = do
  whenM (return $ not $ elem (user ^. #_role) [SP.ADMIN, SP.DRIVER]) $ L.throwException $ err400 {errBody = "NEED_ADMIN_OR_DRIVER_ACCESS"}
  case user ^. #_organizationId of
    Just orgId -> return orgId
    Nothing -> L.throwException $ err400 {errBody = "NO_ORGANIZATION_FOR_THIS_USER"}

addOrgId :: Text -> SV.Vehicle -> L.Flow SV.Vehicle
addOrgId orgId vehicle = return $ vehicle {SV._organizationId = orgId}

validate :: RegToken -> L.Flow Text
validate regToken = do
  SR.RegistrationToken {..} <- QR.verifyToken regToken
  user <- QP.findPersonById (PersonId _EntityId)
  verifyUser user
