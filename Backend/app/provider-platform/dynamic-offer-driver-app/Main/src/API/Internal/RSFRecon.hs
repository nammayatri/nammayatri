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
import qualified SharedLogic.Finance.Reconciliation.Recipes.RsfUtrVsBankDeposit as RsfUtrRecipe
import Storage.Beam.SystemConfigs ()

data BankVerifyReq = BankVerifyReq
  { bankVerifiedAmount :: HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ManualConfirmReq = ManualConfirmReq
  { confirmedBy :: Text,
    reason :: Text
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

confirmOrder :: Id RSO.ReconSettlementOrder -> ManualConfirmReq -> FlowHandler APISuccess
confirmOrder rsoId req = withFlowHandlerAPI $ do
  -- Guards against two concurrent confirm calls for the same row both
  -- passing the "not already confirmed" check before either writes.
  Hedis.withLockRedis ("RsfConfirmLock:" <> getId rsoId) 30 $ do
    rsos <- QRSO.findByIds [getId rsoId]
    rso <- case rsos of
      [] -> throwError $ InvalidRequest "RSO not found"
      (r : _) -> pure r
    when (rso.ourReconStatus == RSO.PAID) $
      throwError $ InvalidRequest "Order already PAID"
    when (rso.reconciliationStatus == Just "SENT") $
      throwError $ InvalidRequest "Order already sent to BAP"
    when (isJust rso.manuallyConfirmedAt) $
      throwError $ InvalidRequest "Order already manually confirmed"
    now <- getCurrentTime
    QRSOExtra.updateManualConfirmation rsoId now req.confirmedBy req.reason
    logInfo $ "RSF manual confirm: orderId=" <> rso.orderId <> " by=" <> req.confirmedBy
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
