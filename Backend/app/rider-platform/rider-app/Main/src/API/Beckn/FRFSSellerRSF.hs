module API.Beckn.FRFSSellerRSF (API, handler) where

import qualified API.Beckn.FRFSSeller.Handler as H
import qualified BecknV2.FRFS.APIs as FRFSSpec
import qualified BecknV2.FRFS.Types as FRFSSpec
import qualified BecknV2.NTS10.APIs as Spec
import qualified BecknV2.NTS10.Types as Spec
import qualified Data.Text as T
import qualified Domain.Action.Beckn.FRFSSeller.Info as DInfo
import qualified Domain.Action.Beckn.FRFSSeller.ReceiverRecon as DRecon
import Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth (SignatureAuthResult)
import Servant hiding (throwError)
import Tools.RsfSignatureAuth (RsfSignatureAuth)

type API =
  Capture "operator" Text
    :> "metro"
    :> "seller"
    :> RsfSignatureAuth 'Domain.PUBLIC_TRANSPORT "Authorization"
    :> ( Spec.ReceiverReconAPI
           :<|> Spec.OnSettleAPI
           :<|> FRFSSpec.InfoAPI
       )

handler :: FlowServer API
handler operator auth = receiverRecon op auth :<|> onSettle op auth :<|> info op auth
  where
    op = T.toLower operator

receiverRecon :: Text -> SignatureAuthResult -> Spec.ReceiverReconReq -> FlowHandler Spec.AckResponse
receiverRecon operator _authResult req = withFlowHandlerAPI $ do
  let ctx = req.receiverReconReqContext
  H.claimOnce
    operator
    "receiver_recon"
    ctx.reconContextTransactionId
    ctx.reconContextMessageId
    (DRecon.handleReceiverRecon operator req)
  pure Spec.ack

onSettle :: Text -> SignatureAuthResult -> Spec.OnSettleReq -> FlowHandler Spec.AckResponse
onSettle operator _authResult req = withFlowHandlerAPI $ do
  let mbMsg = req.onSettleReqMessage
      nested = mbMsg >>= (.onSettleMessageSettlement) >>= (.settlementOrders)
      flat = mbMsg >>= (.onSettleMessageOrders)
  logWarning $
    "FRFS seller on_settle accepted for " <> operator
      <> ": settlementId="
      <> show (mbMsg >>= (.onSettleMessageSettlementId))
      <> " nestedOrders="
      <> show (length <$> nested)
      <> " flatOrders="
      <> show (length <$> flat)
      <> " - ACKNOWLEDGED AND IGNORED: no outbound settle sender exists, so this was not"
      <> " solicited by us and no NPCI state is recorded"
  pure Spec.ack

info :: Text -> SignatureAuthResult -> FRFSSpec.InfoReq -> FlowHandler FRFSSpec.AckResponse
info operator _authResult req = withFlowHandlerAPI $ do
  let ctx = req.infoReqContext
  H.acceptOnce
    operator
    "info"
    ctx.contextTransactionId
    ctx.contextMessageId
    (DInfo.handleInfo operator req)
