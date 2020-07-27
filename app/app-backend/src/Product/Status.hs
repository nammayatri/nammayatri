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
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Models.Case as Case
import qualified Models.ProductInstance as QPI
import Servant
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
      txnId = context ^. #_request_transaction_id
      prodInstId = ProductInstanceId $ req ^. #message . #order . #_id
      state = matchStatus $ req ^. #message . #order . #_state
  status <- case state of
    Just k -> return k
    Nothing -> L.throwException $ err400 {errBody = "INCORRECT STATUS"}
  productInstance <- QPI.findById prodInstId
  orderPi <- QPI.findByParentIdType (Just prodInstId) Case.RIDEORDER
  QPI.updateStatus (orderPi ^. #_id) status
  mkAckResponse txnId "status"

-- Utility Functions

matchStatus :: Text -> Maybe PI.ProductInstanceStatus
matchStatus state = case state of
  "VALID" -> Just PI.VALID
  "INVALID" -> Just PI.INVALID
  "INPROGRESS" -> Just PI.INPROGRESS
  "CONFIRMED" -> Just PI.CONFIRMED
  "COMPLETED" -> Just PI.COMPLETED
  "INSTOCK" -> Just PI.INSTOCK
  "OUTOFSTOCK" -> Just PI.OUTOFSTOCK
  "CANCELLED" -> Just PI.CANCELLED
  "EXPIRED" -> Just PI.EXPIRED
  _ -> Nothing
