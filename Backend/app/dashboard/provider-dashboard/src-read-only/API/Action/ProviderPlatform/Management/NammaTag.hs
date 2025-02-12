{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.NammaTag
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.NammaTag
import qualified Domain.Action.ProviderPlatform.Management.NammaTag
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import qualified Lib.Yudhishthira.Types
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("nammaTag" :> (PostNammaTagTagCreate :<|> PostNammaTagTagVerify :<|> PostNammaTagTagUpdate :<|> DeleteNammaTagTagDelete :<|> PostNammaTagQueryCreate :<|> PostNammaTagQueryUpdate :<|> DeleteNammaTagQueryDelete :<|> PostNammaTagAppDynamicLogicVerify :<|> GetNammaTagAppDynamicLogic :<|> PostNammaTagRunJob :<|> GetNammaTagTimeBounds :<|> PostNammaTagTimeBoundsCreate :<|> DeleteNammaTagTimeBoundsDelete :<|> GetNammaTagAppDynamicLogicGetLogicRollout :<|> PostNammaTagAppDynamicLogicUpsertLogicRollout :<|> GetNammaTagAppDynamicLogicVersions :<|> GetNammaTagAppDynamicLogicDomains :<|> GetNammaTagQueryAll))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postNammaTagTagCreate merchantId city :<|> postNammaTagTagVerify merchantId city :<|> postNammaTagTagUpdate merchantId city :<|> deleteNammaTagTagDelete merchantId city :<|> postNammaTagQueryCreate merchantId city :<|> postNammaTagQueryUpdate merchantId city :<|> deleteNammaTagQueryDelete merchantId city :<|> postNammaTagAppDynamicLogicVerify merchantId city :<|> getNammaTagAppDynamicLogic merchantId city :<|> postNammaTagRunJob merchantId city :<|> getNammaTagTimeBounds merchantId city :<|> postNammaTagTimeBoundsCreate merchantId city :<|> deleteNammaTagTimeBoundsDelete merchantId city :<|> getNammaTagAppDynamicLogicGetLogicRollout merchantId city :<|> postNammaTagAppDynamicLogicUpsertLogicRollout merchantId city :<|> getNammaTagAppDynamicLogicVersions merchantId city :<|> getNammaTagAppDynamicLogicDomains merchantId city :<|> getNammaTagQueryAll merchantId city

type PostNammaTagTagCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.NAMMA_TAG / 'API.Types.ProviderPlatform.Management.NammaTag.POST_NAMMA_TAG_TAG_CREATE)
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagTagCreate
  )

type PostNammaTagTagVerify =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.NAMMA_TAG / 'API.Types.ProviderPlatform.Management.NammaTag.POST_NAMMA_TAG_TAG_VERIFY)
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagTagVerify
  )

type PostNammaTagTagUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.NAMMA_TAG / 'API.Types.ProviderPlatform.Management.NammaTag.POST_NAMMA_TAG_TAG_UPDATE)
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagTagUpdate
  )

type DeleteNammaTagTagDelete =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.NAMMA_TAG / 'API.Types.ProviderPlatform.Management.NammaTag.DELETE_NAMMA_TAG_TAG_DELETE)
      :> API.Types.ProviderPlatform.Management.NammaTag.DeleteNammaTagTagDelete
  )

type PostNammaTagQueryCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.NAMMA_TAG / 'API.Types.ProviderPlatform.Management.NammaTag.POST_NAMMA_TAG_QUERY_CREATE)
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagQueryCreate
  )

type PostNammaTagQueryUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.NAMMA_TAG / 'API.Types.ProviderPlatform.Management.NammaTag.POST_NAMMA_TAG_QUERY_UPDATE)
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagQueryUpdate
  )

type DeleteNammaTagQueryDelete =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.NAMMA_TAG / 'API.Types.ProviderPlatform.Management.NammaTag.DELETE_NAMMA_TAG_QUERY_DELETE)
      :> API.Types.ProviderPlatform.Management.NammaTag.DeleteNammaTagQueryDelete
  )

type PostNammaTagAppDynamicLogicVerify =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.NAMMA_TAG / 'API.Types.ProviderPlatform.Management.NammaTag.POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_VERIFY)
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagAppDynamicLogicVerify
  )

type GetNammaTagAppDynamicLogic =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.NAMMA_TAG / 'API.Types.ProviderPlatform.Management.NammaTag.GET_NAMMA_TAG_APP_DYNAMIC_LOGIC)
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogic
  )

type PostNammaTagRunJob =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.NAMMA_TAG / 'API.Types.ProviderPlatform.Management.NammaTag.POST_NAMMA_TAG_RUN_JOB)
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagRunJob
  )

type GetNammaTagTimeBounds =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.NAMMA_TAG / 'API.Types.ProviderPlatform.Management.NammaTag.GET_NAMMA_TAG_TIME_BOUNDS)
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagTimeBounds
  )

type PostNammaTagTimeBoundsCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.NAMMA_TAG / 'API.Types.ProviderPlatform.Management.NammaTag.POST_NAMMA_TAG_TIME_BOUNDS_CREATE)
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagTimeBoundsCreate
  )

type DeleteNammaTagTimeBoundsDelete =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.NAMMA_TAG / 'API.Types.ProviderPlatform.Management.NammaTag.DELETE_NAMMA_TAG_TIME_BOUNDS_DELETE)
      :> API.Types.ProviderPlatform.Management.NammaTag.DeleteNammaTagTimeBoundsDelete
  )

type GetNammaTagAppDynamicLogicGetLogicRollout =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.NAMMA_TAG / 'API.Types.ProviderPlatform.Management.NammaTag.GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_GET_LOGIC_ROLLOUT)
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogicGetLogicRollout
  )

type PostNammaTagAppDynamicLogicUpsertLogicRollout =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.NAMMA_TAG / 'API.Types.ProviderPlatform.Management.NammaTag.POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_UPSERT_LOGIC_ROLLOUT)
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagAppDynamicLogicUpsertLogicRollout
  )

type GetNammaTagAppDynamicLogicVersions =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.NAMMA_TAG / 'API.Types.ProviderPlatform.Management.NammaTag.GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_VERSIONS)
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogicVersions
  )

type GetNammaTagAppDynamicLogicDomains =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.NAMMA_TAG / 'API.Types.ProviderPlatform.Management.NammaTag.GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_DOMAINS)
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogicDomains
  )

type GetNammaTagQueryAll =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.NAMMA_TAG / 'API.Types.ProviderPlatform.Management.NammaTag.GET_NAMMA_TAG_QUERY_ALL)
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagQueryAll
  )

postNammaTagTagCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.CreateNammaTagRequest -> Environment.FlowHandler Lib.Yudhishthira.Types.CreateNammaTagResponse)
postNammaTagTagCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.postNammaTagTagCreate merchantShortId opCity apiTokenInfo req

postNammaTagTagVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.VerifyNammaTagRequest -> Environment.FlowHandler Lib.Yudhishthira.Types.VerifyNammaTagResponse)
postNammaTagTagVerify merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.postNammaTagTagVerify merchantShortId opCity apiTokenInfo req

postNammaTagTagUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.UpdateNammaTagRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagTagUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.postNammaTagTagUpdate merchantShortId opCity apiTokenInfo req

deleteNammaTagTagDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteNammaTagTagDelete merchantShortId opCity apiTokenInfo tagName = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.deleteNammaTagTagDelete merchantShortId opCity apiTokenInfo tagName

postNammaTagQueryCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.ChakraQueriesAPIEntity -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagQueryCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.postNammaTagQueryCreate merchantShortId opCity apiTokenInfo req

postNammaTagQueryUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.ChakraQueryUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagQueryUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.postNammaTagQueryUpdate merchantShortId opCity apiTokenInfo req

deleteNammaTagQueryDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.ChakraQueryDeleteReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteNammaTagQueryDelete merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.deleteNammaTagQueryDelete merchantShortId opCity apiTokenInfo req

postNammaTagAppDynamicLogicVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.AppDynamicLogicReq -> Environment.FlowHandler Lib.Yudhishthira.Types.AppDynamicLogicResp)
postNammaTagAppDynamicLogicVerify merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.postNammaTagAppDynamicLogicVerify merchantShortId opCity apiTokenInfo req

getNammaTagAppDynamicLogic :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler [Lib.Yudhishthira.Types.GetLogicsResp])
getNammaTagAppDynamicLogic merchantShortId opCity apiTokenInfo version domain = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.getNammaTagAppDynamicLogic merchantShortId opCity apiTokenInfo version domain

postNammaTagRunJob :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.RunKaalChakraJobReq -> Environment.FlowHandler Lib.Yudhishthira.Types.RunKaalChakraJobRes)
postNammaTagRunJob merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.postNammaTagRunJob merchantShortId opCity apiTokenInfo req

getNammaTagTimeBounds :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler Lib.Yudhishthira.Types.TimeBoundResp)
getNammaTagTimeBounds merchantShortId opCity apiTokenInfo domain = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.getNammaTagTimeBounds merchantShortId opCity apiTokenInfo domain

postNammaTagTimeBoundsCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.CreateTimeBoundRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagTimeBoundsCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.postNammaTagTimeBoundsCreate merchantShortId opCity apiTokenInfo req

deleteNammaTagTimeBoundsDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.LogicDomain -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteNammaTagTimeBoundsDelete merchantShortId opCity apiTokenInfo domain name = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.deleteNammaTagTimeBoundsDelete merchantShortId opCity apiTokenInfo domain name

getNammaTagAppDynamicLogicGetLogicRollout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler [Lib.Yudhishthira.Types.LogicRolloutObject])
getNammaTagAppDynamicLogicGetLogicRollout merchantShortId opCity apiTokenInfo timeBound domain = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.getNammaTagAppDynamicLogicGetLogicRollout merchantShortId opCity apiTokenInfo timeBound domain

postNammaTagAppDynamicLogicUpsertLogicRollout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.LogicRolloutReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagAppDynamicLogicUpsertLogicRollout merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.postNammaTagAppDynamicLogicUpsertLogicRollout merchantShortId opCity apiTokenInfo req

getNammaTagAppDynamicLogicVersions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler Lib.Yudhishthira.Types.AppDynamicLogicVersionResp)
getNammaTagAppDynamicLogicVersions merchantShortId opCity apiTokenInfo limit offset domain = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.getNammaTagAppDynamicLogicVersions merchantShortId opCity apiTokenInfo limit offset domain

getNammaTagAppDynamicLogicDomains :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler Lib.Yudhishthira.Types.AppDynamicLogicDomainResp)
getNammaTagAppDynamicLogicDomains merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.getNammaTagAppDynamicLogicDomains merchantShortId opCity apiTokenInfo

getNammaTagQueryAll :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.Chakra -> Environment.FlowHandler Lib.Yudhishthira.Types.ChakraQueryResp)
getNammaTagQueryAll merchantShortId opCity apiTokenInfo chakra = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.getNammaTagQueryAll merchantShortId opCity apiTokenInfo chakra
