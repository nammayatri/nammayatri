{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.CancellationConsequence
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.CancellationConsequence
import qualified Domain.Action.Dashboard.Management.CancellationConsequence
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("cancellationConsequence" :> (GetCancellationConsequenceList :<|> PostCancellationConsequenceCreate :<|> PostCancellationConsequenceUpdate :<|> GetCancellationConsequenceRegistryList :<|> PostCancellationConsequenceRegistryUpsert))

type GetCancellationConsequenceList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/CANCELLATION_CONSEQUENCE/GET_CANCELLATION_CONSEQUENCE_LIST"
      :> API.Types.ProviderPlatform.Management.CancellationConsequence.GetCancellationConsequenceList
  )

type PostCancellationConsequenceCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/CANCELLATION_CONSEQUENCE/POST_CANCELLATION_CONSEQUENCE_CREATE"
      :> API.Types.ProviderPlatform.Management.CancellationConsequence.PostCancellationConsequenceCreate
  )

type PostCancellationConsequenceUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/CANCELLATION_CONSEQUENCE/POST_CANCELLATION_CONSEQUENCE_UPDATE"
      :> API.Types.ProviderPlatform.Management.CancellationConsequence.PostCancellationConsequenceUpdate
  )

type GetCancellationConsequenceRegistryList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/CANCELLATION_CONSEQUENCE/GET_CANCELLATION_CONSEQUENCE_REGISTRY_LIST"
      :> API.Types.ProviderPlatform.Management.CancellationConsequence.GetCancellationConsequenceRegistryList
  )

type PostCancellationConsequenceRegistryUpsert =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/CANCELLATION_CONSEQUENCE/POST_CANCELLATION_CONSEQUENCE_REGISTRY_UPSERT"
      :> API.Types.ProviderPlatform.Management.CancellationConsequence.PostCancellationConsequenceRegistryUpsert
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getCancellationConsequenceList merchantId city :<|> postCancellationConsequenceCreate merchantId city :<|> postCancellationConsequenceUpdate merchantId city :<|> getCancellationConsequenceRegistryList merchantId city :<|> postCancellationConsequenceRegistryUpsert merchantId city

getCancellationConsequenceList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.CancellationConsequence.CancellationConsequenceListRes)
getCancellationConsequenceList a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.CancellationConsequence.getCancellationConsequenceList a5 a4 a2 a1

postCancellationConsequenceCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.CancellationConsequence.CreateCancellationConsequenceReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCancellationConsequenceCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.CancellationConsequence.postCancellationConsequenceCreate a4 a3 a1

postCancellationConsequenceUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.CancellationConsequence.UpdateCancellationConsequenceReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCancellationConsequenceUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.CancellationConsequence.postCancellationConsequenceUpdate a4 a3 a1

getCancellationConsequenceRegistryList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.ProviderPlatform.Management.CancellationConsequence.FaultRuleRegistryListRes)
getCancellationConsequenceRegistryList a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.CancellationConsequence.getCancellationConsequenceRegistryList a3 a2

postCancellationConsequenceRegistryUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.CancellationConsequence.UpsertFaultRuleRegistryReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCancellationConsequenceRegistryUpsert a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.CancellationConsequence.postCancellationConsequenceRegistryUpsert a4 a3 a1
