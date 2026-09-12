{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.NammaTag
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.NammaTag
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.NammaTag
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("nammaTag" :> (PostNammaTagTagCreate :<|> PostNammaTagTagUpdate :<|> PostNammaTagTagVerify :<|> DeleteNammaTagTagDelete :<|> GetNammaTagTagAll :<|> GetNammaTagTagDetails :<|> PostNammaTagQueryCreate :<|> PostNammaTagQueryUpdate :<|> DeleteNammaTagQueryDelete :<|> GetNammaTagQueryDetails :<|> PostNammaTagAppDynamicLogicVerify :<|> GetNammaTagAppDynamicLogic :<|> PostNammaTagRunJob :<|> GetNammaTagTimeBounds :<|> PostNammaTagTimeBoundsCreate :<|> DeleteNammaTagTimeBoundsDelete :<|> GetNammaTagAppDynamicLogicGetLogicRollout :<|> PostNammaTagAppDynamicLogicUpsertLogicRollout :<|> PostNammaTagAppDynamicLogicUpdateExperimentGroup :<|> GetNammaTagAppDynamicLogicExperimentGroups :<|> GetNammaTagAppDynamicLogicVersions :<|> GetNammaTagAppDynamicLogicDomains :<|> GetNammaTagAppDynamicLogicDomainsAndEvents :<|> GetNammaTagAppDynamicLogicGetDomainSchema :<|> GetNammaTagQueryAll :<|> PostNammaTagUpdateCustomerTag :<|> PostNammaTagBulkUpdateCustomerTag :<|> PostNammaTagConfigPilotGetVersion :<|> PostNammaTagConfigPilotGetConfig :<|> PostNammaTagConfigPilotCreateUiConfig :<|> GetNammaTagConfigPilotAllConfigs :<|> GetNammaTagConfigPilotConfigDetails :<|> GetNammaTagConfigPilotGetTableData :<|> GetNammaTagConfigPilotAllUiConfigs :<|> GetNammaTagConfigPilotUiConfigDetails :<|> GetNammaTagConfigPilotGetUiTableData :<|> GetNammaTagConfigPilotAlwaysOnList :<|> PostNammaTagConfigPilotActionChange :<|> PostNammaTagConfigPilotGetConfigWithDimensions :<|> GetNammaTagConfigPilotGetDimensionSchema :<|> PostNammaTagConfigPilotCreateRow))

type PostNammaTagTagCreate = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TAG_CREATE" :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagTagCreate)

type PostNammaTagTagUpdate = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TAG_UPDATE" :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagTagUpdate)

type PostNammaTagTagVerify = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TAG_VERIFY" :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagTagVerify)

type DeleteNammaTagTagDelete =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/DELETE_NAMMA_TAG_TAG_DELETE"
      :> API.Types.RiderPlatform.Management.NammaTag.DeleteNammaTagTagDelete
  )

type GetNammaTagTagAll = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_TAG_ALL" :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagTagAll)

type GetNammaTagTagDetails = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_TAG_DETAILS" :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagTagDetails)

type PostNammaTagQueryCreate =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_QUERY_CREATE"
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagQueryCreate
  )

type PostNammaTagQueryUpdate =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_QUERY_UPDATE"
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagQueryUpdate
  )

type DeleteNammaTagQueryDelete =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/DELETE_NAMMA_TAG_QUERY_DELETE"
      :> API.Types.RiderPlatform.Management.NammaTag.DeleteNammaTagQueryDelete
  )

type GetNammaTagQueryDetails =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_QUERY_DETAILS"
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagQueryDetails
  )

type PostNammaTagAppDynamicLogicVerify =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_VERIFY"
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagAppDynamicLogicVerify
  )

type GetNammaTagAppDynamicLogic =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC"
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogic
  )

type PostNammaTagRunJob = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_RUN_JOB" :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagRunJob)

type GetNammaTagTimeBounds = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_TIME_BOUNDS" :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagTimeBounds)

type PostNammaTagTimeBoundsCreate =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TIME_BOUNDS_CREATE"
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagTimeBoundsCreate
  )

type DeleteNammaTagTimeBoundsDelete =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/DELETE_NAMMA_TAG_TIME_BOUNDS_DELETE"
      :> API.Types.RiderPlatform.Management.NammaTag.DeleteNammaTagTimeBoundsDelete
  )

type GetNammaTagAppDynamicLogicGetLogicRollout =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_GET_LOGIC_ROLLOUT"
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogicGetLogicRollout
  )

type PostNammaTagAppDynamicLogicUpsertLogicRollout =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_UPSERT_LOGIC_ROLLOUT"
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagAppDynamicLogicUpsertLogicRollout
  )

type PostNammaTagAppDynamicLogicUpdateExperimentGroup =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_UPDATE_EXPERIMENT_GROUP"
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagAppDynamicLogicUpdateExperimentGroup
  )

type GetNammaTagAppDynamicLogicExperimentGroups =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_EXPERIMENT_GROUPS"
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogicExperimentGroups
  )

type GetNammaTagAppDynamicLogicVersions =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_VERSIONS"
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogicVersions
  )

type GetNammaTagAppDynamicLogicDomains =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_DOMAINS"
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogicDomains
  )

type GetNammaTagAppDynamicLogicDomainsAndEvents =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_DOMAINS_AND_EVENTS"
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogicDomainsAndEvents
  )

type GetNammaTagAppDynamicLogicGetDomainSchema =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_GET_DOMAIN_SCHEMA"
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogicGetDomainSchema
  )

type GetNammaTagQueryAll = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_QUERY_ALL" :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagQueryAll)

type PostNammaTagUpdateCustomerTag =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_UPDATE_CUSTOMER_TAG"
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagUpdateCustomerTag
  )

type PostNammaTagBulkUpdateCustomerTag =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_BULK_UPDATE_CUSTOMER_TAG"
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagBulkUpdateCustomerTag
  )

type PostNammaTagConfigPilotGetVersion =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_GET_VERSION"
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagConfigPilotGetVersion
  )

type PostNammaTagConfigPilotGetConfig =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_GET_CONFIG"
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagConfigPilotGetConfig
  )

type PostNammaTagConfigPilotCreateUiConfig =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_CREATE_UI_CONFIG"
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagConfigPilotCreateUiConfig
  )

type GetNammaTagConfigPilotAllConfigs =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_ALL_CONFIGS"
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagConfigPilotAllConfigs
  )

type GetNammaTagConfigPilotConfigDetails =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_CONFIG_DETAILS"
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagConfigPilotConfigDetails
  )

type GetNammaTagConfigPilotGetTableData =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_GET_TABLE_DATA"
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagConfigPilotGetTableData
  )

type GetNammaTagConfigPilotAllUiConfigs =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_ALL_UI_CONFIGS"
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagConfigPilotAllUiConfigs
  )

type GetNammaTagConfigPilotUiConfigDetails =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_UI_CONFIG_DETAILS"
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagConfigPilotUiConfigDetails
  )

type GetNammaTagConfigPilotGetUiTableData =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_GET_UI_TABLE_DATA"
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagConfigPilotGetUiTableData
  )

type GetNammaTagConfigPilotAlwaysOnList =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_ALWAYS_ON_LIST"
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagConfigPilotAlwaysOnList
  )

type PostNammaTagConfigPilotActionChange =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_ACTION_CHANGE"
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagConfigPilotActionChange
  )

type PostNammaTagConfigPilotGetConfigWithDimensions =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_GET_CONFIG_WITH_DIMENSIONS"
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagConfigPilotGetConfigWithDimensions
  )

type GetNammaTagConfigPilotGetDimensionSchema =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_GET_DIMENSION_SCHEMA"
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagConfigPilotGetDimensionSchema
  )

type PostNammaTagConfigPilotCreateRow =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_CREATE_ROW"
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagConfigPilotCreateRow
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postNammaTagTagCreate merchantId city :<|> postNammaTagTagUpdate merchantId city :<|> postNammaTagTagVerify merchantId city :<|> deleteNammaTagTagDelete merchantId city :<|> getNammaTagTagAll merchantId city :<|> getNammaTagTagDetails merchantId city :<|> postNammaTagQueryCreate merchantId city :<|> postNammaTagQueryUpdate merchantId city :<|> deleteNammaTagQueryDelete merchantId city :<|> getNammaTagQueryDetails merchantId city :<|> postNammaTagAppDynamicLogicVerify merchantId city :<|> getNammaTagAppDynamicLogic merchantId city :<|> postNammaTagRunJob merchantId city :<|> getNammaTagTimeBounds merchantId city :<|> postNammaTagTimeBoundsCreate merchantId city :<|> deleteNammaTagTimeBoundsDelete merchantId city :<|> getNammaTagAppDynamicLogicGetLogicRollout merchantId city :<|> postNammaTagAppDynamicLogicUpsertLogicRollout merchantId city :<|> postNammaTagAppDynamicLogicUpdateExperimentGroup merchantId city :<|> getNammaTagAppDynamicLogicExperimentGroups merchantId city :<|> getNammaTagAppDynamicLogicVersions merchantId city :<|> getNammaTagAppDynamicLogicDomains merchantId city :<|> getNammaTagAppDynamicLogicDomainsAndEvents merchantId city :<|> getNammaTagAppDynamicLogicGetDomainSchema merchantId city :<|> getNammaTagQueryAll merchantId city :<|> postNammaTagUpdateCustomerTag merchantId city :<|> postNammaTagBulkUpdateCustomerTag merchantId city :<|> postNammaTagConfigPilotGetVersion merchantId city :<|> postNammaTagConfigPilotGetConfig merchantId city :<|> postNammaTagConfigPilotCreateUiConfig merchantId city :<|> getNammaTagConfigPilotAllConfigs merchantId city :<|> getNammaTagConfigPilotConfigDetails merchantId city :<|> getNammaTagConfigPilotGetTableData merchantId city :<|> getNammaTagConfigPilotAllUiConfigs merchantId city :<|> getNammaTagConfigPilotUiConfigDetails merchantId city :<|> getNammaTagConfigPilotGetUiTableData merchantId city :<|> getNammaTagConfigPilotAlwaysOnList merchantId city :<|> postNammaTagConfigPilotActionChange merchantId city :<|> postNammaTagConfigPilotGetConfigWithDimensions merchantId city :<|> getNammaTagConfigPilotGetDimensionSchema merchantId city :<|> postNammaTagConfigPilotCreateRow merchantId city

postNammaTagTagCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.CreateNammaTagRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagTagCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.postNammaTagTagCreate a4 a3 a1

postNammaTagTagUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.UpdateNammaTagRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagTagUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.postNammaTagTagUpdate a4 a3 a1

postNammaTagTagVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.VerifyNammaTagRequest -> Environment.FlowHandler Lib.Yudhishthira.Types.VerifyNammaTagResponse)
postNammaTagTagVerify a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.postNammaTagTagVerify a4 a3 a1

deleteNammaTagTagDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteNammaTagTagDelete a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.deleteNammaTagTagDelete a4 a3 a1

getNammaTagTagAll :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler [Lib.Yudhishthira.Types.NammaTagDetailsResp])
getNammaTagTagAll a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.getNammaTagTagAll a3 a2

getNammaTagTagDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Lib.Yudhishthira.Types.NammaTagDetailsResp)
getNammaTagTagDetails a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.getNammaTagTagDetails a4 a3 a1

postNammaTagQueryCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.ChakraQueriesAPIEntity -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagQueryCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.postNammaTagQueryCreate a4 a3 a1

postNammaTagQueryUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.ChakraQueryUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagQueryUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.postNammaTagQueryUpdate a4 a3 a1

deleteNammaTagQueryDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.ChakraQueryDeleteReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteNammaTagQueryDelete a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.deleteNammaTagQueryDelete a4 a3 a1

getNammaTagQueryDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.Chakra -> Kernel.Prelude.Text -> Environment.FlowHandler Lib.Yudhishthira.Types.ChakraQueriesAPIEntity)
getNammaTagQueryDetails a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.getNammaTagQueryDetails a5 a4 a2 a1

postNammaTagAppDynamicLogicVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.AppDynamicLogicReq -> Environment.FlowHandler Lib.Yudhishthira.Types.AppDynamicLogicResp)
postNammaTagAppDynamicLogicVerify a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.postNammaTagAppDynamicLogicVerify a4 a3 a1

getNammaTagAppDynamicLogic :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler [Lib.Yudhishthira.Types.GetLogicsResp])
getNammaTagAppDynamicLogic a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.getNammaTagAppDynamicLogic a5 a4 a2 a1

postNammaTagRunJob :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.RunKaalChakraJobReq -> Environment.FlowHandler Lib.Yudhishthira.Types.RunKaalChakraJobRes)
postNammaTagRunJob a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.postNammaTagRunJob a4 a3 a1

getNammaTagTimeBounds :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler Lib.Yudhishthira.Types.TimeBoundResp)
getNammaTagTimeBounds a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.getNammaTagTimeBounds a4 a3 a1

postNammaTagTimeBoundsCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.CreateTimeBoundRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagTimeBoundsCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.postNammaTagTimeBoundsCreate a4 a3 a1

deleteNammaTagTimeBoundsDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.LogicDomain -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteNammaTagTimeBoundsDelete a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.deleteNammaTagTimeBoundsDelete a5 a4 a2 a1

getNammaTagAppDynamicLogicGetLogicRollout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler [Lib.Yudhishthira.Types.LogicRolloutObject])
getNammaTagAppDynamicLogicGetLogicRollout a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.getNammaTagAppDynamicLogicGetLogicRollout a6 a5 a3 a2 a1

postNammaTagAppDynamicLogicUpsertLogicRollout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.LogicRolloutReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagAppDynamicLogicUpsertLogicRollout a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.postNammaTagAppDynamicLogicUpsertLogicRollout a4 a3 a1

postNammaTagAppDynamicLogicUpdateExperimentGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.UpdateRolloutGroupReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagAppDynamicLogicUpdateExperimentGroup a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.postNammaTagAppDynamicLogicUpdateExperimentGroup a4 a3 a1

getNammaTagAppDynamicLogicExperimentGroups :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Lib.Yudhishthira.Types.LogicDomain) -> Environment.FlowHandler [Lib.Yudhishthira.Types.RolloutGroupInfo])
getNammaTagAppDynamicLogicExperimentGroups a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.getNammaTagAppDynamicLogicExperimentGroups a4 a3 a1

getNammaTagAppDynamicLogicVersions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler Lib.Yudhishthira.Types.AppDynamicLogicVersionResp)
getNammaTagAppDynamicLogicVersions a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.getNammaTagAppDynamicLogicVersions a6 a5 a3 a2 a1

getNammaTagAppDynamicLogicDomains :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler Lib.Yudhishthira.Types.AppDynamicLogicDomainResp)
getNammaTagAppDynamicLogicDomains a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.getNammaTagAppDynamicLogicDomains a3 a2

getNammaTagAppDynamicLogicDomainsAndEvents :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler Lib.Yudhishthira.Types.NammaTagEventsOrNammaTagNamesResp)
getNammaTagAppDynamicLogicDomainsAndEvents a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.getNammaTagAppDynamicLogicDomainsAndEvents a4 a3 a1

getNammaTagAppDynamicLogicGetDomainSchema :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler Lib.Yudhishthira.Types.DomainSchemaResp)
getNammaTagAppDynamicLogicGetDomainSchema a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.getNammaTagAppDynamicLogicGetDomainSchema a4 a3 a1

getNammaTagQueryAll :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.Chakra -> Environment.FlowHandler Lib.Yudhishthira.Types.ChakraQueryResp)
getNammaTagQueryAll a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.getNammaTagQueryAll a4 a3 a1

postNammaTagUpdateCustomerTag :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.User -> Lib.Yudhishthira.Types.UpdateTagReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagUpdateCustomerTag a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.postNammaTagUpdateCustomerTag a5 a4 a2 a1

postNammaTagBulkUpdateCustomerTag :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.NammaTag.BulkUpdateCustomerTagReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.NammaTag.BulkUpdateCustomerTagRes)
postNammaTagBulkUpdateCustomerTag a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.postNammaTagBulkUpdateCustomerTag a4 a3 a1

postNammaTagConfigPilotGetVersion :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.UiConfigRequest -> Environment.FlowHandler Lib.Yudhishthira.Types.UiConfigGetVersionResponse)
postNammaTagConfigPilotGetVersion a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.postNammaTagConfigPilotGetVersion a4 a3 a1

postNammaTagConfigPilotGetConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.UiConfigRequest -> Environment.FlowHandler Lib.Yudhishthira.Types.UiConfigResponse)
postNammaTagConfigPilotGetConfig a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.postNammaTagConfigPilotGetConfig a4 a3 a1

postNammaTagConfigPilotCreateUiConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.CreateConfigRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagConfigPilotCreateUiConfig a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.postNammaTagConfigPilotCreateUiConfig a4 a3 a1

getNammaTagConfigPilotAllConfigs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler [Lib.Yudhishthira.Types.ConfigType])
getNammaTagConfigPilotAllConfigs a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.getNammaTagConfigPilotAllConfigs a4 a3 a1

getNammaTagConfigPilotConfigDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.ConfigType -> Environment.FlowHandler [Lib.Yudhishthira.Types.ConfigDetailsResp])
getNammaTagConfigPilotConfigDetails a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.getNammaTagConfigPilotConfigDetails a4 a3 a1

getNammaTagConfigPilotGetTableData :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.ConfigType -> Environment.FlowHandler Lib.Yudhishthira.Types.TableDataResp)
getNammaTagConfigPilotGetTableData a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.getNammaTagConfigPilotGetTableData a4 a3 a1

getNammaTagConfigPilotAllUiConfigs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler [Lib.Yudhishthira.Types.LogicDomain])
getNammaTagConfigPilotAllUiConfigs a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.getNammaTagConfigPilotAllUiConfigs a4 a3 a1

getNammaTagConfigPilotUiConfigDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.UiDevicePlatformReq -> Environment.FlowHandler [Lib.Yudhishthira.Types.ConfigDetailsResp])
getNammaTagConfigPilotUiConfigDetails a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.getNammaTagConfigPilotUiConfigDetails a4 a3 a1

getNammaTagConfigPilotGetUiTableData :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.UiDevicePlatformReq -> Environment.FlowHandler Lib.Yudhishthira.Types.TableDataResp)
getNammaTagConfigPilotGetUiTableData a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.getNammaTagConfigPilotGetUiTableData a4 a3 a1

getNammaTagConfigPilotAlwaysOnList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler Lib.Yudhishthira.Types.AlwaysOnListResp)
getNammaTagConfigPilotAlwaysOnList a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.getNammaTagConfigPilotAlwaysOnList a4 a3 a1

postNammaTagConfigPilotActionChange :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.ActionChangeRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagConfigPilotActionChange a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.postNammaTagConfigPilotActionChange a4 a3 a1

postNammaTagConfigPilotGetConfigWithDimensions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.ConfigPilotGetConfigRequest -> Environment.FlowHandler Lib.Yudhishthira.Types.TableDataResp)
postNammaTagConfigPilotGetConfigWithDimensions a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.postNammaTagConfigPilotGetConfigWithDimensions a4 a3 a1

getNammaTagConfigPilotGetDimensionSchema :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.ConfigType -> Environment.FlowHandler Lib.Yudhishthira.Types.DomainSchemaResp)
getNammaTagConfigPilotGetDimensionSchema a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.getNammaTagConfigPilotGetDimensionSchema a4 a3 a1

postNammaTagConfigPilotCreateRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.ConfigPilotCreateRowRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagConfigPilotCreateRow a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.NammaTag.postNammaTagConfigPilotCreateRow a4 a3 a1
