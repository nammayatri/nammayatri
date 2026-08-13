module API.RSF.ReceiverRecon (API, handler) where

import qualified Beckn.ACL.ReceiverRecon as ACL
import qualified BecknV2.RSF.Types as Spec
import qualified BecknV2.RSF.Utils as RSFUtils
import qualified Data.Aeson as A
import qualified Domain.Action.Beckn.ReceiverRecon as DRecon
import Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import qualified Lib.Finance.Storage.Queries.ReconSettlementOrderExtra as QRSOExtra
import Servant hiding (throwError)
import qualified Storage.CachedQueries.Merchant as CQMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

type API =
  "receiver_recon"
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> ReqBody '[JSON] A.Value -- Specifically done to throw NACK instead of JSON error even before reaching handler function
    :> Post '[JSON] Spec.RSFAckResponse

handler :: FlowServer API
handler = receiverRecon

receiverRecon ::
  SignatureAuthResult ->
  A.Value ->
  FlowHandler Spec.RSFAckResponse
receiverRecon _signatureAuthResult rawBody = withFlowHandlerAPI $ do
  case A.fromJSON rawBody of
    A.Success (req :: Spec.ReceiverReconReq) -> receiverReconHandler req
    A.Error err -> do
      logError $ "RSF: receiver_recon malformed body: " <> show err
      pure $ RSFUtils.buildNackForCode RSFUtils.RSFMissingMandatory

receiverReconHandler :: Spec.ReceiverReconReq -> Flow Spec.RSFAckResponse
receiverReconHandler req = do
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
          isDuplicate <- QRSOExtra.messageIdExists msgId
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
                      eDomainReq <- ACL.buildReceiverReconDomain req
                      case eDomainReq of
                        Left nack -> pure nack
                        Right domainReq -> do
                          fork "rsf-receiver-recon" $ do
                            DRecon.ingestReceiverRecon merchant.id moc.id domainReq
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
