module Beckn.ACL.FRFSSeller.OnReceiverRecon (buildOnReceiverReconReq) where

import qualified BecknV2.NTS10.Types as Spec
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Domain.Action.Beckn.FRFSSeller.Recon as Recon
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import qualified SharedLogic.FRFSSeller.Common as Common

buildOnReceiverReconReq ::
  Common.OperatorRecon ->
  (Text, Text) ->
  UTCTime ->
  Spec.ReconContext ->
  [Recon.ReconResult] ->
  Spec.OnReceiverReconReq
buildOnReceiverReconReq recon (selfId, selfUrl) now ctx results =
  Spec.OnReceiverReconReq
    { onReceiverReconReqContext = mkContext recon (selfId, selfUrl) now ctx,
      onReceiverReconReqMessage =
        Spec.OnReceiverReconMessage
          { onReceiverReconMessageOrderbook =
              Spec.OrderbookOut {orderbookOutOrders = map (mkOrder collectorId selfId) results}
          }
    }
  where
    collectorId = fromMaybe "" ctx.reconContextBapId

mkContext :: Common.OperatorRecon -> (Text, Text) -> UTCTime -> Spec.ReconContext -> Spec.ReconContextOut
mkContext recon (selfId, selfUrl) now ctx =
  Spec.ReconContextOut
    { reconContextOutDomain = recon.domain,
      reconContextOutCountry = fromMaybe "IND" ctx.reconContextCountry,
      reconContextOutCity = fromMaybe "" ctx.reconContextCity,
      reconContextOutAction = "on_receiver_recon",
      reconContextOutCoreVersion = recon.coreVersion,
      reconContextOutBapId = fromMaybe "" ctx.reconContextBapId,
      reconContextOutBapUri = fromMaybe "" ctx.reconContextBapUri,
      reconContextOutBppId = selfId,
      reconContextOutBppUri = selfUrl,
      reconContextOutTransactionId = fromMaybe "" ctx.reconContextTransactionId,
      reconContextOutMessageId = fromMaybe "" ctx.reconContextMessageId,
      reconContextOutTimestamp = ondcTimestamp now,
      reconContextOutTtl = recon.ttl
    }

mkOrder :: Text -> Text -> Recon.ReconResult -> Spec.RsfOrderOut
mkOrder collectorId selfId r =
  Spec.RsfOrderOut
    { rsfOrderOutId = r.orderId,
      rsfOrderOutCollectorAppId = collectorId,
      rsfOrderOutReceiverAppId = selfId,
      rsfOrderOutTransactionId = r.echo.echoTransactionId,
      rsfOrderOutSettlementId = r.echo.echoSettlementId,
      rsfOrderOutSettlementReferenceNo = r.echo.echoSettlementReference,
      rsfOrderOutOrderReconStatus = Spec.orderReconStatusFinal,
      rsfOrderOutCounterpartyReconStatus = r.wireStatus,
      rsfOrderOutMessage = Spec.reconMessageFor r.wireStatus,
      rsfOrderOutCounterpartyDiffAmount =
        Spec.NtsPrice {ntsPriceCurrency = "INR", ntsPriceValue = showAmount (abs r.difference)}
    }

showAmount :: HighPrecMoney -> Text
showAmount = Common.formatPrice . realToFrac

ondcTimestamp :: UTCTime -> Text
ondcTimestamp = toText . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.000Z"
