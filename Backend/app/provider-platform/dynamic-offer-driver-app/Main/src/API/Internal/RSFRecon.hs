module API.Internal.RSFRecon (API, handler) where

import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.ReconSettlementOrder as RSO
import qualified Lib.Finance.Domain.Types.ReconUtrSettlement as RUS
import qualified Lib.Finance.Reconciliation.Runner as ReconRunner
import qualified Lib.Finance.Reconciliation.Types as ReconT
import qualified Lib.Finance.Storage.Queries.ReconSettlementOrder as QRSO
import qualified Lib.Finance.Storage.Queries.ReconSettlementOrderExtra as QRSOExtra
import qualified Lib.Finance.Storage.Queries.ReconUtrSettlement as QRUS
import qualified Lib.Finance.Storage.Queries.ReconUtrSettlementExtra as QRUSExtra
import Servant hiding (throwError)
import qualified SharedLogic.CallRSF as CallRSF
import qualified SharedLogic.Finance.Reconciliation.Recipes.RsfBapClaimVsPlatformRide as RsfOrderRecipe
import qualified SharedLogic.Finance.Reconciliation.Recipes.RsfUtrVsBankDeposit as RsfUtrRecipe
import Storage.Beam.SystemConfigs ()

data BankVerifyReq = BankVerifyReq
  { bankVerifiedAmount :: HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- Finance confirms an amount, never a verdict -- see confirmOrder.
data ManualConfirmReq = ManualConfirmReq
  { confirmedBy :: Text,
    reason :: Text,
    confirmedAmount :: HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

type API =
  "rsf"
    :> ( "utrs" :> Capture "utrId" (Id RUS.ReconUtrSettlement)
           :> "bank-verify"
           :> ReqBody '[JSON] BankVerifyReq
           :> Post '[JSON] APISuccess
           :<|> "orders" :> Capture "rsoId" (Id RSO.ReconSettlementOrder)
             :> "confirm"
             :> ReqBody '[JSON] ManualConfirmReq
             :> Post '[JSON] APISuccess
           :<|> "settlements" :> Capture "settlementId" Text
             :> "send"
             :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler = bankVerify :<|> confirmOrder :<|> triggerSend

bankVerify :: Id RUS.ReconUtrSettlement -> BankVerifyReq -> FlowHandler APISuccess
bankVerify utrId req = withFlowHandlerAPI $ do
  bankVerifyAndRecon utrId req.bankVerifiedAmount
  logInfo $ "RSF bank verify: utrId=" <> getId utrId <> " amount=" <> show req.bankVerifiedAmount
  pure Success

bankVerifyAndRecon ::
  Id RUS.ReconUtrSettlement ->
  HighPrecMoney ->
  Flow ()
bankVerifyAndRecon utrId bankVerifiedAmount = do
  QRUSExtra.updateBankVerifiedAmount utrId bankVerifiedAmount

  utr <- QRUS.findById utrId >>= fromMaybeM (InvalidRequest "UTR not found")

  let merchantId = fromMaybe "" utr.merchantId
      merchantOperatingCityId = fromMaybe "" utr.merchantOperatingCityId
      scope = ReconT.MerchantScope merchantId merchantOperatingCityId

  ReconRunner.reconcileSources RsfUtrRecipe.recipe scope [ReconT.SourceId $ getId utrId]

-- | Finance confirms an amount, never a verdict. Does NOT block on
-- ourReconStatus == PAID -- Axis A (wire claim vs fare) never looks at
-- bank-verified truth, so finance must be able to override a false-positive
-- PAID with what they actually know. Blocks when platformGrossFare is
-- Nothing (ride/booking not resolved yet) -- confirming here would silently
-- compute a verdict against a zero fare.
confirmOrder :: Id RSO.ReconSettlementOrder -> ManualConfirmReq -> FlowHandler APISuccess
confirmOrder rsoId req = withFlowHandlerAPI $ do
  Hedis.withLockRedis ("RsfConfirmLock:" <> getId rsoId) 30 $ do
    rsos <- QRSO.findByIds [getId rsoId]
    rso <- case rsos of
      [] -> throwError $ InvalidRequest "RSO not found"
      (r : _) -> pure r
    when (isNothing rso.platformGrossFare) $
      throwError $ InvalidRequest "Ride not resolved yet for this order -- cannot confirm before the fare is known"
    when (rso.reconciliationStatus == Just "SENT") $
      throwError $ InvalidRequest "Order already sent to BAP"
    when (isJust rso.manuallyConfirmedAt) $
      throwError $ InvalidRequest "Order already manually confirmed"
    now <- getCurrentTime

    siblingRows <- QRSO.findByOrderId rso.orderId
    let fare = fromMaybe 0 rso.platformGrossFare
        otherRowsClaimed = sum [RsfOrderRecipe.effectiveClaimedAmount r | r <- siblingRows, r.id /= rso.id]
        totalClaimed = otherRowsClaimed + req.confirmedAmount
        diffAmt = fare - totalClaimed
        verdict
          | diffAmt == 0 = RSO.PAID
          | diffAmt > 0 = RSO.UNDERPAID
          | otherwise = RSO.OVERPAID

    QRSOExtra.updateManualConfirmation rsoId now req.confirmedBy req.reason req.confirmedAmount verdict (Just diffAmt)
    logInfo $ "RSF manual confirm: orderId=" <> rso.orderId <> " by=" <> req.confirmedBy <> " amount=" <> show req.confirmedAmount

    let scope = ReconT.MerchantScope (fromMaybe "" rso.merchantId) (fromMaybe "" rso.merchantOperatingCityId)
    ReconRunner.reconcileSources RsfOrderRecipe.recipe scope [ReconT.SourceId rso.orderId]
  pure Success

triggerSend :: Text -> FlowHandler APISuccess
triggerSend settlementId = withFlowHandlerAPI $ do
  orders <- QRSOExtra.findBySettlementId settlementId
  when (null orders) $
    throwError $ InvalidRequest "Settlement ID not found"
  let rso = head orders
  merchantId <- fromMaybeM (InvalidRequest "Order has no merchantId") (Id <$> rso.merchantId)
  CallRSF.sendOnReceiverRecon merchantId settlementId
  pure Success
