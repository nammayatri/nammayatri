{-# LANGUAGE OverloadedLabels #-}

module Product.Status (status, onStatus) where

import App.Types
import Beckn.Types.Common hiding (status)
import qualified Beckn.Types.Core.API.Status as API
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Models.Case as Case
import qualified Models.ProductInstance as QPI
import qualified Storage.Queries.Organization as OQ
import Types.API.Status as Status
import Types.Error
import qualified Utils.Notifications as Notify
import Utils.Routes

status :: Person.Person -> StatusReq -> FlowHandler StatusRes
status person StatusReq {..} = withFlowHandler $ do
  prodInst <- QPI.findById (Id productInstanceId)
  case_ <- Case.findIdByPerson person (prodInst ^. #_caseId)
  let caseId = getId $ case_ ^. #_id
  msgId <- L.generateGUID
  context <- buildContext "status" caseId msgId
  organization <-
    OQ.findOrganizationById (Id $ prodInst ^. #_organizationId)
      >>= fromMaybeM OrgNotFound
  baseUrl <- organization ^. #_callbackUrl & fromMaybeM OrgCallbackUrlNotSet
  let statusMessage = API.StatusReqMessage (IdObject productInstanceId) (IdObject caseId)
  Gateway.status baseUrl $ API.StatusReq context statusMessage

onStatus :: Organization.Organization -> API.OnStatusReq -> FlowHandler API.OnStatusRes
onStatus _org req = withFlowHandler $ do
  let context = req ^. #context
      txnId = context ^. #_transaction_id
  case req ^. #contents of
    Right msg -> do
      let prodInstId = Id $ msg ^. #order . #_id
          orderState = fromBeckn $ msg ^. #order . #_state
      updateProductInstanceStatus prodInstId orderState
    Left err -> logTagError "on_status req" $ "on_status error: " <> show err
  mkAckResponse txnId "status"
  where
    updateProductInstanceStatus prodInstId piStatus = do
      orderPi <- QPI.findByParentIdType prodInstId Case.RIDEORDER
      QPI.updateStatus (orderPi ^. #_id) piStatus
      Notify.notifyOnStatusUpdate orderPi piStatus
      return ()
