{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.RSFReconciliation
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.RSFReconciliation
import qualified Domain.Action.ProviderPlatform.Management.RSFReconciliation
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import qualified Lib.Finance.Domain.Types.ReconSettlementOrder
import qualified Lib.Finance.Domain.Types.ReconUtrSettlement
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("rSFReconciliation" :> (GetRSFReconciliationRsfSettlements :<|> GetRSFReconciliationRsfSettlementsUtrs :<|> GetRSFReconciliationRsfSettlementsOrders :<|> PostRSFReconciliationRsfSettlementsSend :<|> GetRSFReconciliationRsfUtrs :<|> GetRSFReconciliationRsfUtr :<|> PostRSFReconciliationRsfUtrBankVerify :<|> PostRSFReconciliationRsfOrdersConfirm :<|> GetRSFReconciliationRsfReconGrid :<|> GetRSFReconciliationRsfReconUnmatched))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getRSFReconciliationRsfSettlements merchantId city :<|> getRSFReconciliationRsfSettlementsUtrs merchantId city :<|> getRSFReconciliationRsfSettlementsOrders merchantId city :<|> postRSFReconciliationRsfSettlementsSend merchantId city :<|> getRSFReconciliationRsfUtrs merchantId city :<|> getRSFReconciliationRsfUtr merchantId city :<|> postRSFReconciliationRsfUtrBankVerify merchantId city :<|> postRSFReconciliationRsfOrdersConfirm merchantId city :<|> getRSFReconciliationRsfReconGrid merchantId city :<|> getRSFReconciliationRsfReconUnmatched merchantId city

type GetRSFReconciliationRsfSettlements =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RSF_RECONCILIATION) / ('API.Types.ProviderPlatform.Management.RSFReconciliation.GET_RSF_RECONCILIATION_RSF_SETTLEMENTS))
      :> API.Types.ProviderPlatform.Management.RSFReconciliation.GetRSFReconciliationRsfSettlements
  )

type GetRSFReconciliationRsfSettlementsUtrs =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RSF_RECONCILIATION) / ('API.Types.ProviderPlatform.Management.RSFReconciliation.GET_RSF_RECONCILIATION_RSF_SETTLEMENTS_UTRS))
      :> API.Types.ProviderPlatform.Management.RSFReconciliation.GetRSFReconciliationRsfSettlementsUtrs
  )

type GetRSFReconciliationRsfSettlementsOrders =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RSF_RECONCILIATION) / ('API.Types.ProviderPlatform.Management.RSFReconciliation.GET_RSF_RECONCILIATION_RSF_SETTLEMENTS_ORDERS))
      :> API.Types.ProviderPlatform.Management.RSFReconciliation.GetRSFReconciliationRsfSettlementsOrders
  )

type PostRSFReconciliationRsfSettlementsSend =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RSF_RECONCILIATION) / ('API.Types.ProviderPlatform.Management.RSFReconciliation.POST_RSF_RECONCILIATION_RSF_SETTLEMENTS_SEND))
      :> API.Types.ProviderPlatform.Management.RSFReconciliation.PostRSFReconciliationRsfSettlementsSend
  )

type GetRSFReconciliationRsfUtrs =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RSF_RECONCILIATION) / ('API.Types.ProviderPlatform.Management.RSFReconciliation.GET_RSF_RECONCILIATION_RSF_UTRS))
      :> API.Types.ProviderPlatform.Management.RSFReconciliation.GetRSFReconciliationRsfUtrs
  )

type GetRSFReconciliationRsfUtr =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RSF_RECONCILIATION) / ('API.Types.ProviderPlatform.Management.RSFReconciliation.GET_RSF_RECONCILIATION_RSF_UTR))
      :> API.Types.ProviderPlatform.Management.RSFReconciliation.GetRSFReconciliationRsfUtr
  )

type PostRSFReconciliationRsfUtrBankVerify =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RSF_RECONCILIATION) / ('API.Types.ProviderPlatform.Management.RSFReconciliation.POST_RSF_RECONCILIATION_RSF_UTR_BANK_VERIFY))
      :> API.Types.ProviderPlatform.Management.RSFReconciliation.PostRSFReconciliationRsfUtrBankVerify
  )

type PostRSFReconciliationRsfOrdersConfirm =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RSF_RECONCILIATION) / ('API.Types.ProviderPlatform.Management.RSFReconciliation.POST_RSF_RECONCILIATION_RSF_ORDERS_CONFIRM))
      :> API.Types.ProviderPlatform.Management.RSFReconciliation.PostRSFReconciliationRsfOrdersConfirm
  )

type GetRSFReconciliationRsfReconGrid =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RSF_RECONCILIATION) / ('API.Types.ProviderPlatform.Management.RSFReconciliation.GET_RSF_RECONCILIATION_RSF_RECON_GRID))
      :> API.Types.ProviderPlatform.Management.RSFReconciliation.GetRSFReconciliationRsfReconGrid
  )

type GetRSFReconciliationRsfReconUnmatched =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.RSF_RECONCILIATION) / ('API.Types.ProviderPlatform.Management.RSFReconciliation.GET_RSF_RECONCILIATION_RSF_RECON_UNMATCHED))
      :> API.Types.ProviderPlatform.Management.RSFReconciliation.GetRSFReconciliationRsfReconUnmatched
  )

getRSFReconciliationRsfSettlements :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RSFReconciliation.SettlementBatchListRes)
getRSFReconciliationRsfSettlements merchantShortId opCity apiTokenInfo bapId from limit offset to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RSFReconciliation.getRSFReconciliationRsfSettlements merchantShortId opCity apiTokenInfo bapId from limit offset to

getRSFReconciliationRsfSettlementsUtrs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RSFReconciliation.SettlementBatchUtrListRes)
getRSFReconciliationRsfSettlementsUtrs merchantShortId opCity apiTokenInfo settlementId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RSFReconciliation.getRSFReconciliationRsfSettlementsUtrs merchantShortId opCity apiTokenInfo settlementId

getRSFReconciliationRsfSettlementsOrders :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RSFReconciliation.SettlementBatchOrderListRes)
getRSFReconciliationRsfSettlementsOrders merchantShortId opCity apiTokenInfo settlementId limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RSFReconciliation.getRSFReconciliationRsfSettlementsOrders merchantShortId opCity apiTokenInfo settlementId limit offset

postRSFReconciliationRsfSettlementsSend :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRSFReconciliationRsfSettlementsSend merchantShortId opCity apiTokenInfo settlementId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RSFReconciliation.postRSFReconciliationRsfSettlementsSend merchantShortId opCity apiTokenInfo settlementId

getRSFReconciliationRsfUtrs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RSFReconciliation.UtrListRes)
getRSFReconciliationRsfUtrs merchantShortId opCity apiTokenInfo bapId from isVerified limit offset to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RSFReconciliation.getRSFReconciliationRsfUtrs merchantShortId opCity apiTokenInfo bapId from isVerified limit offset to

getRSFReconciliationRsfUtr :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> (Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RSFReconciliation.UtrDetailRes)
getRSFReconciliationRsfUtr merchantShortId opCity apiTokenInfo utrId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RSFReconciliation.getRSFReconciliationRsfUtr merchantShortId opCity apiTokenInfo utrId

postRSFReconciliationRsfUtrBankVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> (Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement) -> API.Types.ProviderPlatform.Management.RSFReconciliation.BankVerifyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRSFReconciliationRsfUtrBankVerify merchantShortId opCity apiTokenInfo utrId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RSFReconciliation.postRSFReconciliationRsfUtrBankVerify merchantShortId opCity apiTokenInfo utrId req

postRSFReconciliationRsfOrdersConfirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> (Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSettlementOrder) -> API.Types.ProviderPlatform.Management.RSFReconciliation.ManualConfirmReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRSFReconciliationRsfOrdersConfirm merchantShortId opCity apiTokenInfo rsoId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RSFReconciliation.postRSFReconciliationRsfOrdersConfirm merchantShortId opCity apiTokenInfo rsoId req

getRSFReconciliationRsfReconGrid :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.RSFReconciliation.ReconTabStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RSFReconciliation.ReconGridListRes)
getRSFReconciliationRsfReconGrid merchantShortId opCity apiTokenInfo bapId from limit manuallyConfirmedOnly offset status to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RSFReconciliation.getRSFReconciliationRsfReconGrid merchantShortId opCity apiTokenInfo bapId from limit manuallyConfirmedOnly offset status to

getRSFReconciliationRsfReconUnmatched :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RSFReconciliation.ReconGridListRes)
getRSFReconciliationRsfReconUnmatched merchantShortId opCity apiTokenInfo from limit offset to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.RSFReconciliation.getRSFReconciliationRsfReconUnmatched merchantShortId opCity apiTokenInfo from limit offset to
