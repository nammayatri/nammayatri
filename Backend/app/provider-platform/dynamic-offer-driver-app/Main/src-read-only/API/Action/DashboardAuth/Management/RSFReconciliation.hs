{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.RSFReconciliation
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.RSFReconciliation
import qualified Domain.Action.Dashboard.Management.RSFReconciliation
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("rSFReconciliation" :> (GetRSFReconciliationRsfMessages :<|> GetRSFReconciliationRsfMessagesUtrs :<|> GetRSFReconciliationRsfMessagesOrders :<|> PostRSFReconciliationRsfMessagesSend :<|> GetRSFReconciliationRsfUtrs :<|> GetRSFReconciliationRsfUtr :<|> PostRSFReconciliationRsfUtrBankVerify :<|> PostRSFReconciliationRsfOrdersConfirm :<|> GetRSFReconciliationRsfReconGrid :<|> GetRSFReconciliationRsfReconUnmatched))

type GetRSFReconciliationRsfMessages =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_MESSAGES"
      :> API.Types.ProviderPlatform.Management.RSFReconciliation.GetRSFReconciliationRsfMessages
  )

type GetRSFReconciliationRsfMessagesUtrs =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_MESSAGES_UTRS"
      :> API.Types.ProviderPlatform.Management.RSFReconciliation.GetRSFReconciliationRsfMessagesUtrs
  )

type GetRSFReconciliationRsfMessagesOrders =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_MESSAGES_ORDERS"
      :> API.Types.ProviderPlatform.Management.RSFReconciliation.GetRSFReconciliationRsfMessagesOrders
  )

type PostRSFReconciliationRsfMessagesSend =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RSF_RECONCILIATION/POST_RSF_RECONCILIATION_RSF_MESSAGES_SEND"
      :> API.Types.ProviderPlatform.Management.RSFReconciliation.PostRSFReconciliationRsfMessagesSend
  )

type GetRSFReconciliationRsfUtrs =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_UTRS"
      :> API.Types.ProviderPlatform.Management.RSFReconciliation.GetRSFReconciliationRsfUtrs
  )

type GetRSFReconciliationRsfUtr =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_UTR"
      :> API.Types.ProviderPlatform.Management.RSFReconciliation.GetRSFReconciliationRsfUtr
  )

type PostRSFReconciliationRsfUtrBankVerify =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RSF_RECONCILIATION/POST_RSF_RECONCILIATION_RSF_UTR_BANK_VERIFY"
      :> API.Types.ProviderPlatform.Management.RSFReconciliation.PostRSFReconciliationRsfUtrBankVerify
  )

type PostRSFReconciliationRsfOrdersConfirm =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RSF_RECONCILIATION/POST_RSF_RECONCILIATION_RSF_ORDERS_CONFIRM"
      :> API.Types.ProviderPlatform.Management.RSFReconciliation.PostRSFReconciliationRsfOrdersConfirm
  )

type GetRSFReconciliationRsfReconGrid =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_RECON_GRID"
      :> API.Types.ProviderPlatform.Management.RSFReconciliation.GetRSFReconciliationRsfReconGrid
  )

type GetRSFReconciliationRsfReconUnmatched =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RSF_RECONCILIATION/GET_RSF_RECONCILIATION_RSF_RECON_UNMATCHED"
      :> API.Types.ProviderPlatform.Management.RSFReconciliation.GetRSFReconciliationRsfReconUnmatched
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getRSFReconciliationRsfMessages merchantId city :<|> getRSFReconciliationRsfMessagesUtrs merchantId city :<|> getRSFReconciliationRsfMessagesOrders merchantId city :<|> postRSFReconciliationRsfMessagesSend merchantId city :<|> getRSFReconciliationRsfUtrs merchantId city :<|> getRSFReconciliationRsfUtr merchantId city :<|> postRSFReconciliationRsfUtrBankVerify merchantId city :<|> postRSFReconciliationRsfOrdersConfirm merchantId city :<|> getRSFReconciliationRsfReconGrid merchantId city :<|> getRSFReconciliationRsfReconUnmatched merchantId city

getRSFReconciliationRsfMessages :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RSFReconciliation.MessageBatchListRes)
getRSFReconciliationRsfMessages a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a6 $ Domain.Action.Dashboard.Management.RSFReconciliation.getRSFReconciliationRsfMessages a8 a7 a5 a4 a3 a2 a1

getRSFReconciliationRsfMessagesUtrs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RSFReconciliation.MessageBatchUtrListRes)
getRSFReconciliationRsfMessagesUtrs a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.RSFReconciliation.getRSFReconciliationRsfMessagesUtrs a4 a3 a1

getRSFReconciliationRsfMessagesOrders :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RSFReconciliation.MessageBatchOrderListRes)
getRSFReconciliationRsfMessagesOrders a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.Management.RSFReconciliation.getRSFReconciliationRsfMessagesOrders a6 a5 a3 a2 a1

postRSFReconciliationRsfMessagesSend :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRSFReconciliationRsfMessagesSend a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RSF_RECONCILIATION/POST_RSF_RECONCILIATION_RSF_MESSAGES_SEND" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.RSFReconciliation.postRSFReconciliationRsfMessagesSend a4 a3 a1
    )

getRSFReconciliationRsfUtrs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RSFReconciliation.UtrListRes)
getRSFReconciliationRsfUtrs a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a7 $ Domain.Action.Dashboard.Management.RSFReconciliation.getRSFReconciliationRsfUtrs a9 a8 a6 a5 a4 a3 a2 a1

getRSFReconciliationRsfUtr :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RSFReconciliation.UtrDetailRes)
getRSFReconciliationRsfUtr a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.RSFReconciliation.getRSFReconciliationRsfUtr a4 a3 a1

postRSFReconciliationRsfUtrBankVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.RSFReconciliation.BankVerifyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRSFReconciliationRsfUtrBankVerify a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RSF_RECONCILIATION/POST_RSF_RECONCILIATION_RSF_UTR_BANK_VERIFY" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Management.RSFReconciliation.postRSFReconciliationRsfUtrBankVerify a5 a4 a2 a1
    )

postRSFReconciliationRsfOrdersConfirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.RSFReconciliation.ManualConfirmReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRSFReconciliationRsfOrdersConfirm a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/RSF_RECONCILIATION/POST_RSF_RECONCILIATION_RSF_ORDERS_CONFIRM" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Management.RSFReconciliation.postRSFReconciliationRsfOrdersConfirm a5 a4 a2 a1
    )

getRSFReconciliationRsfReconGrid :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.RSFReconciliation.ReconTabStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RSFReconciliation.ReconGridListRes)
getRSFReconciliationRsfReconGrid a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a8 $ Domain.Action.Dashboard.Management.RSFReconciliation.getRSFReconciliationRsfReconGrid a10 a9 a7 a6 a5 a4 a3 a2 a1

getRSFReconciliationRsfReconUnmatched :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RSFReconciliation.ReconGridListRes)
getRSFReconciliationRsfReconUnmatched a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.Management.RSFReconciliation.getRSFReconciliationRsfReconUnmatched a7 a6 a4 a3 a2 a1
