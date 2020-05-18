{-# LANGUAGE OverloadedLabels #-}

module Product.Vehicle where

import Beckn.TypeClass.Transform
import Beckn.Types.App
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.Vehicle as SV
import qualified Beckn.Types.Storage.RegistrationToken as SR
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
  orgId     <- validate token
  vehicle   <- transformFlow req >>= addOrgId orgId
  QV.create vehicle
  return $ CreateVehicleRes vehicle

listVehicles :: Maybe Text -> ListVehicleReq -> FlowHandler ListVehicleRes
listVehicles token req = withFlowHandler $ do
  orgId     <- validate token
  ListVehicleRes <$> (QV.findAllWithLimitOffsetByOrgIds (req ^. #_limit) (req ^. #_offset) [orgId])

-- Core Utility methods are below
verifyAdmin :: SP.Person -> L.Flow Text
verifyAdmin user = do
  whenM (return $ (user ^. #_role) /= SP.ADMIN) $ L.throwException $ err400 {errBody = "NEED_ADMIN_ACCESS"}
  let mOrgId = user ^. #_organizationId
  whenM (return $ isNothing mOrgId) $ L.throwException $ err400 {errBody = "NO_ORGANIZATION_FOR_THIS_USER"}
  return $ fromMaybe "NEVER_SHOULD_BE_HERE" mOrgId

addOrgId :: Text -> SV.Vehicle -> L.Flow SV.Vehicle
addOrgId orgId vehicle = return $ vehicle{SV._organizationId = orgId}

validate :: Maybe Text -> L.Flow Text
validate token = do
  SR.RegistrationToken {..} <- QR.verifyAuth token
  user                      <- QP.findPersonById (PersonId _EntityId)
  verifyAdmin user