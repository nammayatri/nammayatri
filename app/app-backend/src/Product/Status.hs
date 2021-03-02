{-# LANGUAGE OverloadedLabels #-}

module Product.Status (status, onStatus) where

import App.Types
import Beckn.Types.Common hiding (status)
import qualified Beckn.Types.Core.API.Status as API
import Beckn.Types.ID
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Utils.Common (fromMaybeM500, mkAckResponse, withFlowHandler)
import Beckn.Utils.Logging (Log (..))
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Models.Case as Case
import qualified Models.ProductInstance as QPI
import qualified Storage.Queries.Organization as OQ
import Types.API.Status as Status
import qualified Utils.Notifications as Notify
import Utils.Routes

status :: Person.Person -> StatusReq -> FlowHandler StatusRes
status person StatusReq {..} = withFlowHandler $ do
  prodInst <- QPI.findById (ID productInstanceId)
  case_ <- Case.findIdByPerson person (prodInst ^. #_caseId)
  let caseId = getId $ case_ ^. #_id
  msgId <- L.generateGUID
  context <- buildContext "status" caseId msgId
  organization <-
    OQ.findOrganizationById (ID $ prodInst ^. #_organizationId)
      >>= fromMaybeM500 "INVALID_PROVIDER_ID"
  baseUrl <- organization ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED"
  let statusMessage = API.StatusReqMessage (IdObject productInstanceId) (IdObject caseId)
  Gateway.status baseUrl $ API.StatusReq context statusMessage

onStatus :: Organization.Organization -> API.OnStatusReq -> FlowHandler API.OnStatusRes
onStatus _org req = withFlowHandler $ do
  let context = req ^. #context
      txnId = context ^. #_transaction_id
  case req ^. #contents of
    Right msg -> do
      let prodInstId = ID $ msg ^. #order . #_id
          orderState = fromBeckn $ msg ^. #order . #_state
      updateProductInstanceStatus prodInstId orderState
    Left err -> logError "on_status req" $ "on_status error: " <> show err
  mkAckResponse txnId "status"
  where
    updateProductInstanceStatus prodInstId piStatus = do
      orderPi <- QPI.findByParentIdType (Just prodInstId) Case.RIDEORDER
      QPI.updateStatus (orderPi ^. #_id) piStatus
      Notify.notifyOnStatusUpdate orderPi piStatus
      return ()
