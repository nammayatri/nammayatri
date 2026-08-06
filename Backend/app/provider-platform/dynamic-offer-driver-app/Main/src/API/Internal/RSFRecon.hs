module API.Internal.RSFRecon (API, handler) where

import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified SharedLogic.CallRSF as CallRSF
import qualified SharedLogic.RSFLedger as RSFLedger
import Storage.Beam.SystemConfigs ()

data BankVerifyReq = BankVerifyReq
  { bankVerifiedAmount :: HighPrecMoney,
    verifiedBy :: Maybe Text,
    reason :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- confirmedAmount is what the order should hold from this UTR after the re-split.
data ManualConfirmReq = ManualConfirmReq
  { utr :: Text,
    confirmedBy :: Text,
    reason :: Text,
    confirmedAmount :: HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- Internal twins of the dashboard RSF endpoints: Phase 3 (bank-verify, manual re-split) and Phase 4 (send).
type API =
  "rsf"
    :> Capture "merchantId" (Id DM.Merchant)
    :> ( "utrs" :> Capture "utr" Text
           :> "bank-verify"
           :> ReqBody '[JSON] BankVerifyReq
           :> Post '[JSON] APISuccess
           :<|> "orders" :> Capture "orderId" Text
             :> "confirm"
             :> ReqBody '[JSON] ManualConfirmReq
             :> Post '[JSON] APISuccess
           :<|> "messages" :> Capture "messageId" Text
             :> "send"
             :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler merchantId = bankVerify merchantId :<|> confirmOrder merchantId :<|> triggerSend merchantId

bankVerify :: Id DM.Merchant -> Text -> BankVerifyReq -> FlowHandler APISuccess
bankVerify merchantId utr req = withFlowHandlerAPI $ do
  RSFLedger.verifyUtr merchantId.getId utr req.bankVerifiedAmount req.verifiedBy req.reason
  logInfo $ "RSF bank verify: utr=" <> utr <> " amount=" <> show req.bankVerifiedAmount
  pure Success

confirmOrder :: Id DM.Merchant -> Text -> ManualConfirmReq -> FlowHandler APISuccess
confirmOrder merchantId orderId req = withFlowHandlerAPI $ do
  RSFLedger.reallocateOrder merchantId.getId orderId req.utr req.confirmedAmount req.confirmedBy req.reason
  logInfo $ "RSF manual allocation: orderId=" <> orderId <> " utr=" <> req.utr <> " by=" <> req.confirmedBy <> " amount=" <> show req.confirmedAmount
  pure Success

triggerSend :: Id DM.Merchant -> Text -> FlowHandler APISuccess
triggerSend merchantId messageId = withFlowHandlerAPI $ do
  CallRSF.sendOnReceiverRecon merchantId messageId
  pure Success
