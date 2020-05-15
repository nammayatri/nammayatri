{-# LANGUAGE OverloadedLabels #-}

module Product.Vehicle where

import Beckn.TypeClass.Transform
import Beckn.Types.App
import qualified Beckn.Types.Storage.Person as SP
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
  SR.RegistrationToken {..} <- QR.verifyAuth token
  verifyAdmin (req ^. #_organizationId) _EntityId
  vehicle <- transformFlow req
  QV.create vehicle
  return $ CreateVehicleRes vehicle

listVehicles :: Maybe Text -> ListVehicleReq -> FlowHandler ListVehicleRes
listVehicles token req = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth token
  verifyAdmin (req ^. #_organizationId) _EntityId
  ListVehicleRes <$> (QV.findAllWithLimitOffsetByOrgIds (req ^. #_limit) (req ^. #_offset) [req ^. #_organizationId])

verifyAdmin orgId entityId = do
  user <- QP.findPersonById (PersonId entityId)
  whenM (return $ SP._role user /= SP.ADMIN) $ L.throwException $ err400 {errBody = "NEED_ADMIN_ACCESS"}
  whenM (return $ (Just orgId) /= user ^. #_organizationId) $ L.throwException $ err400 {errBody = "USER_NOT_BELONG_TO_ORGANIZATION"}
