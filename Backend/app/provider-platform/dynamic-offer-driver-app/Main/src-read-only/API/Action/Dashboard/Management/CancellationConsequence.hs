{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.CancellationConsequence
  ( API.Types.ProviderPlatform.Management.CancellationConsequence.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.CancellationConsequence.API)
handler merchantId city = getCancellationConsequenceList merchantId city :<|> postCancellationConsequenceCreate merchantId city :<|> postCancellationConsequenceUpdate merchantId city :<|> getCancellationConsequenceRegistryList merchantId city :<|> postCancellationConsequenceRegistryUpsert merchantId city

getCancellationConsequenceList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.CancellationConsequence.CancellationConsequenceListRes)
getCancellationConsequenceList a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.CancellationConsequence.getCancellationConsequenceList a4 a3 a2 a1

postCancellationConsequenceCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.CancellationConsequence.CreateCancellationConsequenceReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCancellationConsequenceCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.CancellationConsequence.postCancellationConsequenceCreate a3 a2 a1

postCancellationConsequenceUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.CancellationConsequence.UpdateCancellationConsequenceReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCancellationConsequenceUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.CancellationConsequence.postCancellationConsequenceUpdate a3 a2 a1

getCancellationConsequenceRegistryList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.ProviderPlatform.Management.CancellationConsequence.FaultRuleRegistryListRes)
getCancellationConsequenceRegistryList a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.CancellationConsequence.getCancellationConsequenceRegistryList a2 a1

postCancellationConsequenceRegistryUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.CancellationConsequence.UpsertFaultRuleRegistryReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCancellationConsequenceRegistryUpsert a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.CancellationConsequence.postCancellationConsequenceRegistryUpsert a3 a2 a1
