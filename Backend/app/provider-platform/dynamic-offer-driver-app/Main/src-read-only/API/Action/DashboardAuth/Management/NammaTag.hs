{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.NammaTag
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.NammaTag
import qualified Domain.Action.Dashboard.Management.NammaTag
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.BehaviorTracker.Types
import qualified Lib.Yudhishthira.Types
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("nammaTag" :> (PostNammaTagTagCreate :<|> PostNammaTagTagVerify :<|> PostNammaTagTagUpdate :<|> DeleteNammaTagTagDelete :<|> GetNammaTagTagAll :<|> GetNammaTagTagDetails :<|> PostNammaTagQueryCreate :<|> PostNammaTagQueryUpdate :<|> DeleteNammaTagQueryDelete :<|> GetNammaTagQueryDetails :<|> PostNammaTagAppDynamicLogicVerify :<|> GetNammaTagAppDynamicLogic :<|> PostNammaTagRunJob :<|> GetNammaTagTimeBounds :<|> PostNammaTagTimeBoundsCreate :<|> DeleteNammaTagTimeBoundsDelete :<|> GetNammaTagAppDynamicLogicGetLogicRollout :<|> PostNammaTagAppDynamicLogicUpsertLogicRollout :<|> PostNammaTagAppDynamicLogicUpdateExperimentGroup :<|> GetNammaTagAppDynamicLogicExperimentGroups :<|> GetNammaTagAppDynamicLogicVersions :<|> GetNammaTagAppDynamicLogicDomains :<|> GetNammaTagAppDynamicLogicDomainsAndEvents :<|> GetNammaTagAppDynamicLogicGetDomainSchema :<|> GetNammaTagQueryAll :<|> PostNammaTagConfigPilotGetVersion :<|> PostNammaTagConfigPilotGetConfig :<|> PostNammaTagConfigPilotCreateUiConfig :<|> GetNammaTagConfigPilotAllConfigs :<|> GetNammaTagConfigPilotConfigDetails :<|> GetNammaTagConfigPilotGetTableData :<|> GetNammaTagConfigPilotAllUiConfigs :<|> GetNammaTagConfigPilotUiConfigDetails :<|> GetNammaTagConfigPilotGetUiTableData :<|> GetNammaTagConfigPilotAlwaysOnList :<|> PostNammaTagConfigPilotActionChange :<|> PostNammaTagConfigPilotGetPatchedElement :<|> PostNammaTagConfigPilotGetConfigWithDimensions :<|> GetNammaTagConfigPilotGetDimensionSchema :<|> PostNammaTagConfigPilotCreateRow :<|> GetNammaTagBehaviorVisibility))

type PostNammaTagTagCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TAG_CREATE"
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagTagCreate
  )

type PostNammaTagTagVerify =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TAG_VERIFY"
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagTagVerify
  )

type PostNammaTagTagUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TAG_UPDATE"
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagTagUpdate
  )

type DeleteNammaTagTagDelete =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/DELETE_NAMMA_TAG_TAG_DELETE"
      :> API.Types.ProviderPlatform.Management.NammaTag.DeleteNammaTagTagDelete
  )

type GetNammaTagTagAll = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_TAG_ALL" :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagTagAll)

type GetNammaTagTagDetails =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_TAG_DETAILS"
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagTagDetails
  )

type PostNammaTagQueryCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_QUERY_CREATE"
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagQueryCreate
  )

type PostNammaTagQueryUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_QUERY_UPDATE"
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagQueryUpdate
  )

type DeleteNammaTagQueryDelete =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/DELETE_NAMMA_TAG_QUERY_DELETE"
      :> API.Types.ProviderPlatform.Management.NammaTag.DeleteNammaTagQueryDelete
  )

type GetNammaTagQueryDetails =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_QUERY_DETAILS"
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagQueryDetails
  )

type PostNammaTagAppDynamicLogicVerify =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_VERIFY"
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagAppDynamicLogicVerify
  )

type GetNammaTagAppDynamicLogic =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC"
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogic
  )

type PostNammaTagRunJob = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_RUN_JOB" :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagRunJob)

type GetNammaTagTimeBounds =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_TIME_BOUNDS"
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagTimeBounds
  )

type PostNammaTagTimeBoundsCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TIME_BOUNDS_CREATE"
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagTimeBoundsCreate
  )

type DeleteNammaTagTimeBoundsDelete =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/DELETE_NAMMA_TAG_TIME_BOUNDS_DELETE"
      :> API.Types.ProviderPlatform.Management.NammaTag.DeleteNammaTagTimeBoundsDelete
  )

type GetNammaTagAppDynamicLogicGetLogicRollout =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_GET_LOGIC_ROLLOUT"
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogicGetLogicRollout
  )

type PostNammaTagAppDynamicLogicUpsertLogicRollout =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_UPSERT_LOGIC_ROLLOUT"
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagAppDynamicLogicUpsertLogicRollout
  )

type PostNammaTagAppDynamicLogicUpdateExperimentGroup =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_UPDATE_EXPERIMENT_GROUP"
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagAppDynamicLogicUpdateExperimentGroup
  )

type GetNammaTagAppDynamicLogicExperimentGroups =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_EXPERIMENT_GROUPS"
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogicExperimentGroups
  )

type GetNammaTagAppDynamicLogicVersions =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_VERSIONS"
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogicVersions
  )

type GetNammaTagAppDynamicLogicDomains =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_DOMAINS"
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogicDomains
  )

type GetNammaTagAppDynamicLogicDomainsAndEvents =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_DOMAINS_AND_EVENTS"
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogicDomainsAndEvents
  )

type GetNammaTagAppDynamicLogicGetDomainSchema =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_GET_DOMAIN_SCHEMA"
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogicGetDomainSchema
  )

type GetNammaTagQueryAll =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_QUERY_ALL"
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagQueryAll
  )

type PostNammaTagConfigPilotGetVersion =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_GET_VERSION"
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagConfigPilotGetVersion
  )

type PostNammaTagConfigPilotGetConfig =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_GET_CONFIG"
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagConfigPilotGetConfig
  )

type PostNammaTagConfigPilotCreateUiConfig =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_CREATE_UI_CONFIG"
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagConfigPilotCreateUiConfig
  )

type GetNammaTagConfigPilotAllConfigs =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_ALL_CONFIGS"
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagConfigPilotAllConfigs
  )

type GetNammaTagConfigPilotConfigDetails =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_CONFIG_DETAILS"
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagConfigPilotConfigDetails
  )

type GetNammaTagConfigPilotGetTableData =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_GET_TABLE_DATA"
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagConfigPilotGetTableData
  )

type GetNammaTagConfigPilotAllUiConfigs =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_ALL_UI_CONFIGS"
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagConfigPilotAllUiConfigs
  )

type GetNammaTagConfigPilotUiConfigDetails =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_UI_CONFIG_DETAILS"
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagConfigPilotUiConfigDetails
  )

type GetNammaTagConfigPilotGetUiTableData =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_GET_UI_TABLE_DATA"
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagConfigPilotGetUiTableData
  )

type GetNammaTagConfigPilotAlwaysOnList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_ALWAYS_ON_LIST"
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagConfigPilotAlwaysOnList
  )

type PostNammaTagConfigPilotActionChange =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_ACTION_CHANGE"
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagConfigPilotActionChange
  )

type PostNammaTagConfigPilotGetPatchedElement =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_GET_PATCHED_ELEMENT"
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagConfigPilotGetPatchedElement
  )

type PostNammaTagConfigPilotGetConfigWithDimensions =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_GET_CONFIG_WITH_DIMENSIONS"
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagConfigPilotGetConfigWithDimensions
  )

type GetNammaTagConfigPilotGetDimensionSchema =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_GET_DIMENSION_SCHEMA"
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagConfigPilotGetDimensionSchema
  )

type PostNammaTagConfigPilotCreateRow =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_CREATE_ROW"
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagConfigPilotCreateRow
  )

type GetNammaTagBehaviorVisibility =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_BEHAVIOR_VISIBILITY"
      :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagBehaviorVisibility
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postNammaTagTagCreate merchantId city :<|> postNammaTagTagVerify merchantId city :<|> postNammaTagTagUpdate merchantId city :<|> deleteNammaTagTagDelete merchantId city :<|> getNammaTagTagAll merchantId city :<|> getNammaTagTagDetails merchantId city :<|> postNammaTagQueryCreate merchantId city :<|> postNammaTagQueryUpdate merchantId city :<|> deleteNammaTagQueryDelete merchantId city :<|> getNammaTagQueryDetails merchantId city :<|> postNammaTagAppDynamicLogicVerify merchantId city :<|> getNammaTagAppDynamicLogic merchantId city :<|> postNammaTagRunJob merchantId city :<|> getNammaTagTimeBounds merchantId city :<|> postNammaTagTimeBoundsCreate merchantId city :<|> deleteNammaTagTimeBoundsDelete merchantId city :<|> getNammaTagAppDynamicLogicGetLogicRollout merchantId city :<|> postNammaTagAppDynamicLogicUpsertLogicRollout merchantId city :<|> postNammaTagAppDynamicLogicUpdateExperimentGroup merchantId city :<|> getNammaTagAppDynamicLogicExperimentGroups merchantId city :<|> getNammaTagAppDynamicLogicVersions merchantId city :<|> getNammaTagAppDynamicLogicDomains merchantId city :<|> getNammaTagAppDynamicLogicDomainsAndEvents merchantId city :<|> getNammaTagAppDynamicLogicGetDomainSchema merchantId city :<|> getNammaTagQueryAll merchantId city :<|> postNammaTagConfigPilotGetVersion merchantId city :<|> postNammaTagConfigPilotGetConfig merchantId city :<|> postNammaTagConfigPilotCreateUiConfig merchantId city :<|> getNammaTagConfigPilotAllConfigs merchantId city :<|> getNammaTagConfigPilotConfigDetails merchantId city :<|> getNammaTagConfigPilotGetTableData merchantId city :<|> getNammaTagConfigPilotAllUiConfigs merchantId city :<|> getNammaTagConfigPilotUiConfigDetails merchantId city :<|> getNammaTagConfigPilotGetUiTableData merchantId city :<|> getNammaTagConfigPilotAlwaysOnList merchantId city :<|> postNammaTagConfigPilotActionChange merchantId city :<|> postNammaTagConfigPilotGetPatchedElement merchantId city :<|> postNammaTagConfigPilotGetConfigWithDimensions merchantId city :<|> getNammaTagConfigPilotGetDimensionSchema merchantId city :<|> postNammaTagConfigPilotCreateRow merchantId city :<|> getNammaTagBehaviorVisibility merchantId city

postNammaTagTagCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.CreateNammaTagRequest -> Environment.FlowHandler Lib.Yudhishthira.Types.CreateNammaTagResponse)
postNammaTagTagCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagTagCreate a4 a3 a1

postNammaTagTagVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.VerifyNammaTagRequest -> Environment.FlowHandler Lib.Yudhishthira.Types.VerifyNammaTagResponse)
postNammaTagTagVerify a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagTagVerify a4 a3 a1

postNammaTagTagUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.UpdateNammaTagRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagTagUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagTagUpdate a4 a3 a1

deleteNammaTagTagDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteNammaTagTagDelete a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.deleteNammaTagTagDelete a4 a3 a1

getNammaTagTagAll :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler [Lib.Yudhishthira.Types.NammaTagDetailsResp])
getNammaTagTagAll a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagTagAll a3 a2

getNammaTagTagDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Lib.Yudhishthira.Types.NammaTagDetailsResp)
getNammaTagTagDetails a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagTagDetails a4 a3 a1

postNammaTagQueryCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.ChakraQueriesAPIEntity -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagQueryCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagQueryCreate a4 a3 a1

postNammaTagQueryUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.ChakraQueryUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagQueryUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagQueryUpdate a4 a3 a1

deleteNammaTagQueryDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.ChakraQueryDeleteReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteNammaTagQueryDelete a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.deleteNammaTagQueryDelete a4 a3 a1

getNammaTagQueryDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.Chakra -> Kernel.Prelude.Text -> Environment.FlowHandler Lib.Yudhishthira.Types.ChakraQueriesAPIEntity)
getNammaTagQueryDetails a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagQueryDetails a5 a4 a2 a1

postNammaTagAppDynamicLogicVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.AppDynamicLogicReq -> Environment.FlowHandler Lib.Yudhishthira.Types.AppDynamicLogicResp)
postNammaTagAppDynamicLogicVerify a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagAppDynamicLogicVerify a4 a3 a1

getNammaTagAppDynamicLogic :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler [Lib.Yudhishthira.Types.GetLogicsResp])
getNammaTagAppDynamicLogic a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagAppDynamicLogic a5 a4 a2 a1

postNammaTagRunJob :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.RunKaalChakraJobReq -> Environment.FlowHandler Lib.Yudhishthira.Types.RunKaalChakraJobRes)
postNammaTagRunJob a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagRunJob a4 a3 a1

getNammaTagTimeBounds :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler Lib.Yudhishthira.Types.TimeBoundResp)
getNammaTagTimeBounds a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagTimeBounds a4 a3 a1

postNammaTagTimeBoundsCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.CreateTimeBoundRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagTimeBoundsCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagTimeBoundsCreate a4 a3 a1

deleteNammaTagTimeBoundsDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.LogicDomain -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteNammaTagTimeBoundsDelete a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.deleteNammaTagTimeBoundsDelete a5 a4 a2 a1

getNammaTagAppDynamicLogicGetLogicRollout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler [Lib.Yudhishthira.Types.LogicRolloutObject])
getNammaTagAppDynamicLogicGetLogicRollout a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagAppDynamicLogicGetLogicRollout a6 a5 a3 a2 a1

postNammaTagAppDynamicLogicUpsertLogicRollout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.LogicRolloutReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagAppDynamicLogicUpsertLogicRollout a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagAppDynamicLogicUpsertLogicRollout a4 a3 a1

postNammaTagAppDynamicLogicUpdateExperimentGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.UpdateRolloutGroupReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagAppDynamicLogicUpdateExperimentGroup a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagAppDynamicLogicUpdateExperimentGroup a4 a3 a1

getNammaTagAppDynamicLogicExperimentGroups :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Lib.Yudhishthira.Types.LogicDomain) -> Environment.FlowHandler [Lib.Yudhishthira.Types.RolloutGroupInfo])
getNammaTagAppDynamicLogicExperimentGroups a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagAppDynamicLogicExperimentGroups a4 a3 a1

getNammaTagAppDynamicLogicVersions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler Lib.Yudhishthira.Types.AppDynamicLogicVersionResp)
getNammaTagAppDynamicLogicVersions a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagAppDynamicLogicVersions a6 a5 a3 a2 a1

getNammaTagAppDynamicLogicDomains :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler Lib.Yudhishthira.Types.AppDynamicLogicDomainResp)
getNammaTagAppDynamicLogicDomains a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagAppDynamicLogicDomains a3 a2

getNammaTagAppDynamicLogicDomainsAndEvents :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler Lib.Yudhishthira.Types.NammaTagEventsOrNammaTagNamesResp)
getNammaTagAppDynamicLogicDomainsAndEvents a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagAppDynamicLogicDomainsAndEvents a4 a3 a1

getNammaTagAppDynamicLogicGetDomainSchema :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler Lib.Yudhishthira.Types.DomainSchemaResp)
getNammaTagAppDynamicLogicGetDomainSchema a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagAppDynamicLogicGetDomainSchema a4 a3 a1

getNammaTagQueryAll :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.Chakra -> Environment.FlowHandler Lib.Yudhishthira.Types.ChakraQueryResp)
getNammaTagQueryAll a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagQueryAll a4 a3 a1

postNammaTagConfigPilotGetVersion :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.UiConfigRequest -> Environment.FlowHandler Lib.Yudhishthira.Types.UiConfigGetVersionResponse)
postNammaTagConfigPilotGetVersion a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagConfigPilotGetVersion a4 a3 a1

postNammaTagConfigPilotGetConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.UiConfigRequest -> Environment.FlowHandler Lib.Yudhishthira.Types.UiConfigResponse)
postNammaTagConfigPilotGetConfig a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagConfigPilotGetConfig a4 a3 a1

postNammaTagConfigPilotCreateUiConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.CreateConfigRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagConfigPilotCreateUiConfig a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagConfigPilotCreateUiConfig a4 a3 a1

getNammaTagConfigPilotAllConfigs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler [Lib.Yudhishthira.Types.ConfigType])
getNammaTagConfigPilotAllConfigs a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagConfigPilotAllConfigs a4 a3 a1

getNammaTagConfigPilotConfigDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.ConfigType -> Environment.FlowHandler [Lib.Yudhishthira.Types.ConfigDetailsResp])
getNammaTagConfigPilotConfigDetails a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagConfigPilotConfigDetails a4 a3 a1

getNammaTagConfigPilotGetTableData :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.ConfigType -> Environment.FlowHandler Lib.Yudhishthira.Types.TableDataResp)
getNammaTagConfigPilotGetTableData a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagConfigPilotGetTableData a4 a3 a1

getNammaTagConfigPilotAllUiConfigs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler [Lib.Yudhishthira.Types.LogicDomain])
getNammaTagConfigPilotAllUiConfigs a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagConfigPilotAllUiConfigs a4 a3 a1

getNammaTagConfigPilotUiConfigDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.UiDevicePlatformReq -> Environment.FlowHandler [Lib.Yudhishthira.Types.ConfigDetailsResp])
getNammaTagConfigPilotUiConfigDetails a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagConfigPilotUiConfigDetails a4 a3 a1

getNammaTagConfigPilotGetUiTableData :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.UiDevicePlatformReq -> Environment.FlowHandler Lib.Yudhishthira.Types.TableDataResp)
getNammaTagConfigPilotGetUiTableData a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagConfigPilotGetUiTableData a4 a3 a1

getNammaTagConfigPilotAlwaysOnList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler Lib.Yudhishthira.Types.AlwaysOnListResp)
getNammaTagConfigPilotAlwaysOnList a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagConfigPilotAlwaysOnList a4 a3 a1

postNammaTagConfigPilotActionChange :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.ActionChangeRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagConfigPilotActionChange a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagConfigPilotActionChange a4 a3 a1

postNammaTagConfigPilotGetPatchedElement :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.GetPatchedElementReq -> Environment.FlowHandler Lib.Yudhishthira.Types.GetPatchedElementResp)
postNammaTagConfigPilotGetPatchedElement a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagConfigPilotGetPatchedElement a4 a3 a1

postNammaTagConfigPilotGetConfigWithDimensions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.ConfigPilotGetConfigRequest -> Environment.FlowHandler Lib.Yudhishthira.Types.TableDataResp)
postNammaTagConfigPilotGetConfigWithDimensions a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagConfigPilotGetConfigWithDimensions a4 a3 a1

getNammaTagConfigPilotGetDimensionSchema :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.ConfigType -> Environment.FlowHandler Lib.Yudhishthira.Types.DomainSchemaResp)
getNammaTagConfigPilotGetDimensionSchema a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagConfigPilotGetDimensionSchema a4 a3 a1

postNammaTagConfigPilotCreateRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.ConfigPilotCreateRowRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagConfigPilotCreateRow a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagConfigPilotCreateRow a4 a3 a1

getNammaTagBehaviorVisibility :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler Lib.BehaviorTracker.Types.EntityBehaviorVisibility)
getNammaTagBehaviorVisibility a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagBehaviorVisibility a5 a4 a2 a1
