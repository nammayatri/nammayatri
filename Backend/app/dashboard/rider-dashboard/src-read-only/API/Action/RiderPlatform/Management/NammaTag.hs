{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.NammaTag
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.NammaTag
import qualified Dashboard.Common
import qualified Domain.Action.RiderPlatform.Management.NammaTag
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("nammaTag" :> (PostNammaTagTagCreate :<|> PostNammaTagTagUpdate :<|> DeleteNammaTagTagDelete :<|> PostNammaTagQueryCreate :<|> PostNammaTagQueryUpdate :<|> DeleteNammaTagQueryDelete :<|> PostNammaTagAppDynamicLogicVerify :<|> GetNammaTagAppDynamicLogic :<|> PostNammaTagRunJob :<|> GetNammaTagTimeBounds :<|> PostNammaTagTimeBoundsCreate :<|> DeleteNammaTagTimeBoundsDelete :<|> GetNammaTagAppDynamicLogicGetLogicRollout :<|> PostNammaTagAppDynamicLogicUpsertLogicRollout :<|> GetNammaTagAppDynamicLogicVersions :<|> GetNammaTagAppDynamicLogicDomains :<|> GetNammaTagQueryAll :<|> PostNammaTagUpdateCustomerTag :<|> PostNammaTagConfigPilotGetVersion :<|> PostNammaTagConfigPilotGetConfig :<|> PostNammaTagConfigPilotCreateUiConfig :<|> GetNammaTagConfigPilotAllConfigs :<|> GetNammaTagConfigPilotConfigDetails :<|> GetNammaTagConfigPilotGetTableData :<|> PostNammaTagConfigPilotActionChange))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postNammaTagTagCreate merchantId city :<|> postNammaTagTagUpdate merchantId city :<|> deleteNammaTagTagDelete merchantId city :<|> postNammaTagQueryCreate merchantId city :<|> postNammaTagQueryUpdate merchantId city :<|> deleteNammaTagQueryDelete merchantId city :<|> postNammaTagAppDynamicLogicVerify merchantId city :<|> getNammaTagAppDynamicLogic merchantId city :<|> postNammaTagRunJob merchantId city :<|> getNammaTagTimeBounds merchantId city :<|> postNammaTagTimeBoundsCreate merchantId city :<|> deleteNammaTagTimeBoundsDelete merchantId city :<|> getNammaTagAppDynamicLogicGetLogicRollout merchantId city :<|> postNammaTagAppDynamicLogicUpsertLogicRollout merchantId city :<|> getNammaTagAppDynamicLogicVersions merchantId city :<|> getNammaTagAppDynamicLogicDomains merchantId city :<|> getNammaTagQueryAll merchantId city :<|> postNammaTagUpdateCustomerTag merchantId city :<|> postNammaTagConfigPilotGetVersion merchantId city :<|> postNammaTagConfigPilotGetConfig merchantId city :<|> postNammaTagConfigPilotCreateUiConfig merchantId city :<|> getNammaTagConfigPilotAllConfigs merchantId city :<|> getNammaTagConfigPilotConfigDetails merchantId city :<|> getNammaTagConfigPilotGetTableData merchantId city :<|> postNammaTagConfigPilotActionChange merchantId city

type PostNammaTagTagCreate =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.POST_NAMMA_TAG_TAG_CREATE))
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagTagCreate
  )

type PostNammaTagTagUpdate =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.POST_NAMMA_TAG_TAG_UPDATE))
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagTagUpdate
  )

type DeleteNammaTagTagDelete =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.DELETE_NAMMA_TAG_TAG_DELETE))
      :> API.Types.RiderPlatform.Management.NammaTag.DeleteNammaTagTagDelete
  )

type PostNammaTagQueryCreate =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.POST_NAMMA_TAG_QUERY_CREATE))
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagQueryCreate
  )

type PostNammaTagQueryUpdate =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.POST_NAMMA_TAG_QUERY_UPDATE))
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagQueryUpdate
  )

type DeleteNammaTagQueryDelete =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.DELETE_NAMMA_TAG_QUERY_DELETE))
      :> API.Types.RiderPlatform.Management.NammaTag.DeleteNammaTagQueryDelete
  )

type PostNammaTagAppDynamicLogicVerify =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_VERIFY))
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagAppDynamicLogicVerify
  )

type GetNammaTagAppDynamicLogic =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.GET_NAMMA_TAG_APP_DYNAMIC_LOGIC))
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogic
  )

type PostNammaTagRunJob =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.POST_NAMMA_TAG_RUN_JOB))
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagRunJob
  )

type GetNammaTagTimeBounds =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.GET_NAMMA_TAG_TIME_BOUNDS))
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagTimeBounds
  )

type PostNammaTagTimeBoundsCreate =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.POST_NAMMA_TAG_TIME_BOUNDS_CREATE))
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagTimeBoundsCreate
  )

type DeleteNammaTagTimeBoundsDelete =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.DELETE_NAMMA_TAG_TIME_BOUNDS_DELETE))
      :> API.Types.RiderPlatform.Management.NammaTag.DeleteNammaTagTimeBoundsDelete
  )

type GetNammaTagAppDynamicLogicGetLogicRollout =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_GET_LOGIC_ROLLOUT))
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogicGetLogicRollout
  )

type PostNammaTagAppDynamicLogicUpsertLogicRollout =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_UPSERT_LOGIC_ROLLOUT))
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagAppDynamicLogicUpsertLogicRollout
  )

type GetNammaTagAppDynamicLogicVersions =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_VERSIONS))
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogicVersions
  )

type GetNammaTagAppDynamicLogicDomains =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_DOMAINS))
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogicDomains
  )

type GetNammaTagQueryAll =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.GET_NAMMA_TAG_QUERY_ALL))
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagQueryAll
  )

type PostNammaTagUpdateCustomerTag =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.POST_NAMMA_TAG_UPDATE_CUSTOMER_TAG))
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagUpdateCustomerTag
  )

type PostNammaTagConfigPilotGetVersion =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.POST_NAMMA_TAG_CONFIG_PILOT_GET_VERSION))
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagConfigPilotGetVersion
  )

type PostNammaTagConfigPilotGetConfig =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.POST_NAMMA_TAG_CONFIG_PILOT_GET_CONFIG))
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagConfigPilotGetConfig
  )

type PostNammaTagConfigPilotCreateUiConfig =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.POST_NAMMA_TAG_CONFIG_PILOT_CREATE_UI_CONFIG))
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagConfigPilotCreateUiConfig
  )

type GetNammaTagConfigPilotAllConfigs =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.GET_NAMMA_TAG_CONFIG_PILOT_ALL_CONFIGS))
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagConfigPilotAllConfigs
  )

type GetNammaTagConfigPilotConfigDetails =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.GET_NAMMA_TAG_CONFIG_PILOT_CONFIG_DETAILS))
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagConfigPilotConfigDetails
  )

type GetNammaTagConfigPilotGetTableData =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.NAMMA_TAG) / ('API.Types.RiderPlatform.Management.NammaTag.GET_NAMMA_TAG_CONFIG_PILOT_GET_TABLE_DATA))
      :> API.Types.RiderPlatform.Management.NammaTag.GetNammaTagConfigPilotGetTableData
  )

type PostNammaTagConfigPilotActionChange =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.NAMMA_TAG / 'API.Types.RiderPlatform.Management.NammaTag.POST_NAMMA_TAG_CONFIG_PILOT_ACTION_CHANGE)
      :> API.Types.RiderPlatform.Management.NammaTag.PostNammaTagConfigPilotActionChange
  )

postNammaTagTagCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.CreateNammaTagRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagTagCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.postNammaTagTagCreate merchantShortId opCity apiTokenInfo req

postNammaTagTagUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.UpdateNammaTagRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagTagUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.postNammaTagTagUpdate merchantShortId opCity apiTokenInfo req

deleteNammaTagTagDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteNammaTagTagDelete merchantShortId opCity apiTokenInfo tagName = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.deleteNammaTagTagDelete merchantShortId opCity apiTokenInfo tagName

postNammaTagQueryCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.ChakraQueriesAPIEntity -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagQueryCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.postNammaTagQueryCreate merchantShortId opCity apiTokenInfo req

postNammaTagQueryUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.ChakraQueryUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagQueryUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.postNammaTagQueryUpdate merchantShortId opCity apiTokenInfo req

deleteNammaTagQueryDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.ChakraQueryDeleteReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteNammaTagQueryDelete merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.deleteNammaTagQueryDelete merchantShortId opCity apiTokenInfo req

postNammaTagAppDynamicLogicVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.AppDynamicLogicReq -> Environment.FlowHandler Lib.Yudhishthira.Types.AppDynamicLogicResp)
postNammaTagAppDynamicLogicVerify merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.postNammaTagAppDynamicLogicVerify merchantShortId opCity apiTokenInfo req

getNammaTagAppDynamicLogic :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler [Lib.Yudhishthira.Types.GetLogicsResp])
getNammaTagAppDynamicLogic merchantShortId opCity apiTokenInfo version domain = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.getNammaTagAppDynamicLogic merchantShortId opCity apiTokenInfo version domain

postNammaTagRunJob :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.RunKaalChakraJobReq -> Environment.FlowHandler Lib.Yudhishthira.Types.RunKaalChakraJobRes)
postNammaTagRunJob merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.postNammaTagRunJob merchantShortId opCity apiTokenInfo req

getNammaTagTimeBounds :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler Lib.Yudhishthira.Types.TimeBoundResp)
getNammaTagTimeBounds merchantShortId opCity apiTokenInfo domain = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.getNammaTagTimeBounds merchantShortId opCity apiTokenInfo domain

postNammaTagTimeBoundsCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.CreateTimeBoundRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagTimeBoundsCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.postNammaTagTimeBoundsCreate merchantShortId opCity apiTokenInfo req

deleteNammaTagTimeBoundsDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.LogicDomain -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteNammaTagTimeBoundsDelete merchantShortId opCity apiTokenInfo domain name = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.deleteNammaTagTimeBoundsDelete merchantShortId opCity apiTokenInfo domain name

getNammaTagAppDynamicLogicGetLogicRollout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler [Lib.Yudhishthira.Types.LogicRolloutObject])
getNammaTagAppDynamicLogicGetLogicRollout merchantShortId opCity apiTokenInfo timeBound domain = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.getNammaTagAppDynamicLogicGetLogicRollout merchantShortId opCity apiTokenInfo timeBound domain

postNammaTagAppDynamicLogicUpsertLogicRollout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.LogicRolloutReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagAppDynamicLogicUpsertLogicRollout merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.postNammaTagAppDynamicLogicUpsertLogicRollout merchantShortId opCity apiTokenInfo req

getNammaTagAppDynamicLogicVersions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler Lib.Yudhishthira.Types.AppDynamicLogicVersionResp)
getNammaTagAppDynamicLogicVersions merchantShortId opCity apiTokenInfo limit offset domain = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.getNammaTagAppDynamicLogicVersions merchantShortId opCity apiTokenInfo limit offset domain

getNammaTagAppDynamicLogicDomains :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler Lib.Yudhishthira.Types.AppDynamicLogicDomainResp)
getNammaTagAppDynamicLogicDomains merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.getNammaTagAppDynamicLogicDomains merchantShortId opCity apiTokenInfo

getNammaTagQueryAll :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.Chakra -> Environment.FlowHandler Lib.Yudhishthira.Types.ChakraQueryResp)
getNammaTagQueryAll merchantShortId opCity apiTokenInfo chakra = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.getNammaTagQueryAll merchantShortId opCity apiTokenInfo chakra

postNammaTagUpdateCustomerTag :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.User -> Lib.Yudhishthira.Types.UpdateTagReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagUpdateCustomerTag merchantShortId opCity apiTokenInfo customerId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.postNammaTagUpdateCustomerTag merchantShortId opCity apiTokenInfo customerId req

postNammaTagConfigPilotGetVersion :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.UiConfigRequest -> Environment.FlowHandler Kernel.Prelude.Text)
postNammaTagConfigPilotGetVersion merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.postNammaTagConfigPilotGetVersion merchantShortId opCity apiTokenInfo req

postNammaTagConfigPilotGetConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.UiConfigRequest -> Environment.FlowHandler Lib.Yudhishthira.Types.UiConfigResponse)
postNammaTagConfigPilotGetConfig merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.postNammaTagConfigPilotGetConfig merchantShortId opCity apiTokenInfo req

postNammaTagConfigPilotCreateUiConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.CreateConfigRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagConfigPilotCreateUiConfig merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.postNammaTagConfigPilotCreateUiConfig merchantShortId opCity apiTokenInfo req

getNammaTagConfigPilotAllConfigs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler [Lib.Yudhishthira.Types.ConfigType])
getNammaTagConfigPilotAllConfigs merchantShortId opCity apiTokenInfo underExperiment = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.getNammaTagConfigPilotAllConfigs merchantShortId opCity apiTokenInfo underExperiment

getNammaTagConfigPilotConfigDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.ConfigType -> Environment.FlowHandler [Lib.Yudhishthira.Types.ConfigDetailsResp])
getNammaTagConfigPilotConfigDetails merchantShortId opCity apiTokenInfo tableName = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.getNammaTagConfigPilotConfigDetails merchantShortId opCity apiTokenInfo tableName

getNammaTagConfigPilotGetTableData :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.ConfigType -> Environment.FlowHandler Lib.Yudhishthira.Types.TableDataResp)
getNammaTagConfigPilotGetTableData merchantShortId opCity apiTokenInfo tableName = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.getNammaTagConfigPilotGetTableData merchantShortId opCity apiTokenInfo tableName

postNammaTagConfigPilotActionChange :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.ActionChangeRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagConfigPilotActionChange merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.NammaTag.postNammaTagConfigPilotActionChange merchantShortId opCity apiTokenInfo req
