module API.Beckn.ReceiverRecon (API, handler) where

import qualified Beckn.ACL.ReceiverRecon as ACL
import qualified BecknV2.RSF.Types as Spec
import qualified BecknV2.RSF.Utils as RSFUtils
import qualified Domain.Action.Beckn.ReceiverRecon as DRecon
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import qualified Lib.Finance.Storage.Queries.ReconSettlementOrder as QRSO
import Servant hiding (throwError)

type API =
  "receiver_recon"
    :> Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> ReqBody '[JSON] Spec.ReceiverReconReq
    :> Post '[JSON] Spec.RSFAckResponse

handler :: FlowServer API
handler = receiverRecon

receiverRecon ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Spec.ReceiverReconReq ->
  FlowHandler Spec.RSFAckResponse
receiverRecon merchantId _ req = withFlowHandlerAPI $ do
  let ctx = req.receiverReconReqContext
  case validateContext ctx of
    Just nack -> pure nack
    Nothing -> do
      let messageId = ctx.rsfContextMessageId
      case messageId of
        Nothing -> do
          logError "RSF: receiver_recon missing message_id"
          pure $ RSFUtils.buildNackForCode RSFUtils.RSFMissingMandatory
        Just msgId -> do
          isDuplicate <- QRSO.messageIdExists msgId
          if isDuplicate
            then do
              logWarning $ "RSF: duplicate messageId=" <> msgId
              pure $ RSFUtils.buildNackForCode RSFUtils.RSFDuplicateMessage
            else do
              domainReq <- ACL.buildReceiverReconDomain req
              fork "rsf-receiver-recon" $ do
                DRecon.handleReceiverRecon merchantId domainReq
              pure RSFUtils.buildAck

validateContext :: Spec.RSFContext -> Maybe Spec.RSFAckResponse
validateContext ctx
  | ctx.rsfContextDomain /= Just "ONDC:NTS10" =
    Just $ RSFUtils.buildNackForCode RSFUtils.RSFInvalidDomain
  | ctx.rsfContextAction /= Just "receiver_recon" =
    Just $ RSFUtils.buildNackForCode RSFUtils.RSFInvalidAction
  | otherwise = Nothing
