module API.RSF.ReceiverRecon (API, handler) where

import qualified Beckn.ACL.ReceiverRecon as ACL
import qualified BecknV2.RSF.Types as Spec
import qualified BecknV2.RSF.Utils as RSFUtils
import qualified Domain.Action.Beckn.ReceiverRecon as DRecon
import Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import qualified Lib.Finance.Storage.Queries.ReconSettlementOrder as QRSO
import Servant hiding (throwError)
import qualified Storage.CachedQueries.Merchant as CQMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

type API =
  "receiver_recon"
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> ReqBody '[JSON] Spec.ReceiverReconReq
    :> Post '[JSON] Spec.RSFAckResponse

handler :: FlowServer API
handler = receiverRecon

receiverRecon ::
  SignatureAuthResult ->
  Spec.ReceiverReconReq ->
  FlowHandler Spec.RSFAckResponse
receiverRecon _authResult req = withFlowHandlerAPI $ do
  let ctx = req.receiverReconReqContext
  case validateContext ctx of
    Just nack -> pure nack
    Nothing -> do
      let messageId = ctx.rsfContextMessageId
          mbBppId = ctx.rsfContextBppId
      case (messageId, mbBppId) of
        (Nothing, _) -> do
          logError "RSF: receiver_recon missing message_id"
          pure $ RSFUtils.buildNackForCode RSFUtils.RSFMissingMandatory
        (_, Nothing) -> do
          logError "RSF: receiver_recon missing bpp_id"
          pure $ RSFUtils.buildNackForCode RSFUtils.RSFMissingMandatory
        (Just msgId, Just bppId) -> do
          isDuplicate <- QRSO.messageIdExists msgId
          if isDuplicate
            then do
              logWarning $ "RSF: duplicate messageId=" <> msgId
              pure $ RSFUtils.buildNackForCode RSFUtils.RSFDuplicateMessage
            else do
              mbMerchant <- CQMerchant.findBySubscriberId (ShortId bppId)
              case mbMerchant of
                Nothing -> do
                  logError $ "RSF: no merchant found for bpp_id=" <> bppId
                  pure $ RSFUtils.buildNack ("No merchant found for bpp_id: " <> bppId)
                Just merchant -> do
                  mbMoc <- CQMOC.findByMerchantIdAndCity merchant.id merchant.city
                  case mbMoc of
                    Nothing -> do
                      logError $ "RSF: no operating city for merchant=" <> bppId
                      pure $ RSFUtils.buildNack "No operating city found for merchant"
                    Just moc -> do
                      domainReq <- ACL.buildReceiverReconDomain req
                      DRecon.ingestReceiverRecon merchant.id moc.id domainReq
                      fork "rsf-receiver-recon" $
                        DRecon.reconcileIngestedOrders merchant.id moc.id domainReq
                      pure RSFUtils.buildAck

validateContext :: Spec.RSFContext -> Maybe Spec.RSFAckResponse
validateContext ctx
  | ctx.rsfContextDomain /= Just "ONDC:NTS10" =
    Just $ RSFUtils.buildNackForCode RSFUtils.RSFInvalidDomain
  | ctx.rsfContextAction /= Just "receiver_recon" =
    Just $ RSFUtils.buildNackForCode RSFUtils.RSFInvalidAction
  | ctx.rsfContextCoreVersion /= Just "1.0.0" =
    Just $ RSFUtils.buildNackForCode RSFUtils.RSFInvalidVersion
  | otherwise = Nothing
