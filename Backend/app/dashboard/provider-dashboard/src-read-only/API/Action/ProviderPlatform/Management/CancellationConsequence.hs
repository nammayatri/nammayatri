{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.CancellationConsequence
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.CancellationConsequence
import qualified Domain.Action.ProviderPlatform.Management.CancellationConsequence
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("cancellationConsequence" :> (GetCancellationConsequenceList :<|> PostCancellationConsequenceCreate :<|> PostCancellationConsequenceUpdate :<|> GetCancellationConsequenceRegistryList :<|> PostCancellationConsequenceRegistryUpsert))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getCancellationConsequenceList merchantId city :<|> postCancellationConsequenceCreate merchantId city :<|> postCancellationConsequenceUpdate merchantId city :<|> getCancellationConsequenceRegistryList merchantId city :<|> postCancellationConsequenceRegistryUpsert merchantId city

type GetCancellationConsequenceList =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.CANCELLATION_CONSEQUENCE) / ('API.Types.ProviderPlatform.Management.CancellationConsequence.GET_CANCELLATION_CONSEQUENCE_LIST))
      :> API.Types.ProviderPlatform.Management.CancellationConsequence.GetCancellationConsequenceList
  )

type PostCancellationConsequenceCreate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.CANCELLATION_CONSEQUENCE) / ('API.Types.ProviderPlatform.Management.CancellationConsequence.POST_CANCELLATION_CONSEQUENCE_CREATE))
      :> API.Types.ProviderPlatform.Management.CancellationConsequence.PostCancellationConsequenceCreate
  )

type PostCancellationConsequenceUpdate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.CANCELLATION_CONSEQUENCE) / ('API.Types.ProviderPlatform.Management.CancellationConsequence.POST_CANCELLATION_CONSEQUENCE_UPDATE))
      :> API.Types.ProviderPlatform.Management.CancellationConsequence.PostCancellationConsequenceUpdate
  )

type GetCancellationConsequenceRegistryList =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.CANCELLATION_CONSEQUENCE) / ('API.Types.ProviderPlatform.Management.CancellationConsequence.GET_CANCELLATION_CONSEQUENCE_REGISTRY_LIST))
      :> API.Types.ProviderPlatform.Management.CancellationConsequence.GetCancellationConsequenceRegistryList
  )

type PostCancellationConsequenceRegistryUpsert =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.CANCELLATION_CONSEQUENCE) / ('API.Types.ProviderPlatform.Management.CancellationConsequence.POST_CANCELLATION_CONSEQUENCE_REGISTRY_UPSERT))
      :> API.Types.ProviderPlatform.Management.CancellationConsequence.PostCancellationConsequenceRegistryUpsert
  )

getCancellationConsequenceList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.CancellationConsequence.CancellationConsequenceListRes)
getCancellationConsequenceList merchantShortId opCity apiTokenInfo limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.CancellationConsequence.getCancellationConsequenceList merchantShortId opCity apiTokenInfo limit offset

postCancellationConsequenceCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.CancellationConsequence.CreateCancellationConsequenceReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCancellationConsequenceCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.CancellationConsequence.postCancellationConsequenceCreate merchantShortId opCity apiTokenInfo req

postCancellationConsequenceUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.CancellationConsequence.UpdateCancellationConsequenceReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCancellationConsequenceUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.CancellationConsequence.postCancellationConsequenceUpdate merchantShortId opCity apiTokenInfo req

getCancellationConsequenceRegistryList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler API.Types.ProviderPlatform.Management.CancellationConsequence.FaultRuleRegistryListRes)
getCancellationConsequenceRegistryList merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.CancellationConsequence.getCancellationConsequenceRegistryList merchantShortId opCity apiTokenInfo

postCancellationConsequenceRegistryUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.CancellationConsequence.UpsertFaultRuleRegistryReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCancellationConsequenceRegistryUpsert merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.CancellationConsequence.postCancellationConsequenceRegistryUpsert merchantShortId opCity apiTokenInfo req
