{-# LANGUAGE OverloadedLabels #-}

module Product.Status where

import App.Types
import qualified Beckn.Types.API.Status as API
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as PI
import Beckn.Utils.Common (mkAckResponse, withFlowHandler)
import qualified Data.Text as T
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.ProductInstance as QPI
import Types.API.Status as Status
import Utils.Routes

status :: Person.Person -> StatusReq -> FlowHandler StatusRes
status person StatusReq {..} = withFlowHandler $ do
  pi <- QPI.findById (ProductInstanceId productInstanceId)
  case_ <- Case.findIdByPerson person (pi ^. #_caseId)
  let caseId = _getCaseId $ case_ ^. #_id
  context <- buildContext "status" caseId
  baseUrl <- Gateway.getBaseUrl
  let statusMessage = API.StatusReqMessage (IdObject productInstanceId) (IdObject caseId)
  eres <- Gateway.status baseUrl $ API.StatusReq context statusMessage
  let ack =
        case eres of
          Left err -> Ack "status" ("Err: " <> show err)
          Right _ -> Ack "status" "Ok"
  return $ AckResponse context ack Nothing

onStatus :: API.OnStatusReq -> FlowHandler API.OnStatusRes
onStatus req = withFlowHandler $ do
  let context = req ^. #context
      txnId = context ^. #_transaction_id
      prodInstId = ProductInstanceId $ req ^. #message . #order . #_id
      state = req ^. #message . #order . #_state
      status = read (T.unpack state) :: PI.ProductInstanceStatus
  productInstance <- QPI.findById prodInstId
  orderPi <- QPI.findByParentIdType (Just prodInstId) Case.RIDEORDER
  QPI.updateStatus (orderPi ^. #_id) status
  mkAckResponse txnId "status"
