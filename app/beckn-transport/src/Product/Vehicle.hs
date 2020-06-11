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

createVehicle :: Maybe Text -> CreateVehicleReq -> FlowHandler CreateVehicleRes
createVehicle token req = withFlowHandler $ do
  orgId <- validate token
  vehicle <- transformFlow req >>= addOrgId orgId
  QV.create vehicle
  return $ CreateVehicleRes vehicle

listVehicles :: Maybe Text -> ListVehicleReq -> FlowHandler ListVehicleRes
listVehicles token req = withFlowHandler $ do
  orgId <- validate token
  ListVehicleRes <$> (QV.findAllWithLimitOffsetByOrgIds (req ^. #_limit) (req ^. #_offset) [orgId])

updateVehicle :: Text -> Maybe Text -> UpdateVehicleReq -> FlowHandler UpdateVehicleRes
updateVehicle vehicleId token req = withFlowHandler $ do
  orgId <- validate token
  vehicle <- QV.findByIdAndOrgId (VehicleId {_getVechicleId = vehicleId}) orgId
  updatedVehicle <- transformFlow2 req vehicle
  QV.updateVehicleRec updatedVehicle
  return $ CreateVehicleRes {vehicle = updatedVehicle}

getVehicle :: Maybe Text -> Text -> SV.RegistrationCategory -> FlowHandler CreateVehicleRes
getVehicle token regNo regCategory = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth token
  user <- QP.findPersonById (PersonId _EntityId)
  vehicle <- QV.findByRegNoCategory regNo regCategory
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
  let mOrgId = user ^. #_organizationId
  whenM (return $ isNothing mOrgId) $ L.throwException $ err400 {errBody = "NO_ORGANIZATION_FOR_THIS_USER"}
  return $ fromMaybe "NEVER_SHOULD_BE_HERE" mOrgId

addOrgId :: Text -> SV.Vehicle -> L.Flow SV.Vehicle
addOrgId orgId vehicle = return $ vehicle {SV._organizationId = orgId}

validate :: Maybe Text -> L.Flow Text
validate token = do
  SR.RegistrationToken {..} <- QR.verifyAuth token
  user <- QP.findPersonById (PersonId _EntityId)
  verifyUser user
