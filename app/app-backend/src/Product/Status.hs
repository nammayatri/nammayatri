{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Status where

import App.Types
import qualified Beckn.Types.API.Status as API
import Beckn.Types.App
import Beckn.Types.Common
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
  piid <- QPI.findById (ProductInstanceId productInstanceId)
  case_ <- Case.findIdByPerson person (piid ^. #_caseId)
  let caseId = _getCaseId $ case_ ^. #_id
  context <- buildContext "status" caseId
  baseUrl <- Gateway.getProviderBaseUrl
  let statusMessage = API.StatusReqMessage (IdObject productInstanceId) (IdObject caseId)
  Gateway.status baseUrl $ API.StatusReq context statusMessage

onStatus :: API.OnStatusReq -> FlowHandler API.OnStatusRes
onStatus req = withFlowHandler $ do
  let context = req ^. #context
      txnId = context ^. #_transaction_id
  case req ^. #contents of
    Right msg -> do
      let prodInstId = ProductInstanceId $ msg ^. #order . #_id
          orderState = matchStatus $ msg ^. #order . #_state
      orderStatus <- case orderState of
        Just k -> return k
        Nothing -> L.throwException $ err400 {errBody = "INCORRECT STATUS"}
      orderPi <- QPI.findByParentIdType (Just prodInstId) Case.RIDEORDER
      QPI.updateStatus (orderPi ^. #_id) orderStatus
    Left err -> L.logError @Text "on_status req" $ "on_status error: " <> show err
  mkAckResponse txnId "status"

-- Utility Functions

matchStatus :: Text -> Maybe PI.ProductInstanceStatus
matchStatus piStatus = case piStatus of
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
