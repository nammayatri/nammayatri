module Product.Confirm
  ( confirm,
    ConfirmAPI,
    onConfirm,
  )
where

import App.Types
import qualified Beckn.Storage.Esqueleto as DB
import qualified Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.APISuccess
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as OnConfirm
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.Confirm as ACL
import qualified Core.ACL.OnConfirm as ACL
import Data.Aeson (encode)
import qualified Domain.Action.Beckn.OnConfirm as DOnConfirm
import qualified Domain.Action.UI.Confirm as DConfirm
import qualified Domain.Types.Person as SP
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import Servant
import Utils.Auth
import Utils.Common

type ConfirmAPI =
  "rideSearch"
    :> TokenAuth
    :> "confirm"
    :> ReqBody '[JSON] DConfirm.ConfirmReq
    :> Post '[JSON] APISuccess

confirm ::
  Id SP.Person ->
  DConfirm.ConfirmReq ->
  FlowHandler APISuccess
confirm personId req =
  withFlowHandlerAPI . withPersonIdLogTag personId $ do
    dConfirmRes <- DConfirm.confirm personId req
    void . ExternalAPI.confirm dConfirmRes.bppUrl =<< ACL.buildConfirmReq dConfirmRes
    return Success

onConfirm ::
  SignatureAuthResult ->
  OnConfirm.OnConfirmReq ->
  FlowHandler AckResponse
onConfirm (SignatureAuthResult signPayload _) req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnConfirmReq <- ACL.buildOnConfirmReq req
  DB.runTransaction $ do
    QBR.logBecknRequest (show $ encode req) (show $ signPayload.signature)
  whenJust mbDOnConfirmReq DOnConfirm.onConfirm
  pure Ack
