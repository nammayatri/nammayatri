{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.NammaTag where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Yudhishthira.Types
import Servant
import Servant.Client

type API = ("nammaTag" :> (PostNammaTagTagCreate :<|> PostNammaTagTagUpdate :<|> DeleteNammaTagTagDelete :<|> PostNammaTagQueryCreate :<|> PostNammaTagQueryUpdate :<|> DeleteNammaTagQueryDelete :<|> PostNammaTagAppDynamicLogicVerify :<|> GetNammaTagAppDynamicLogic :<|> PostNammaTagRunJob :<|> GetNammaTagTimeBounds :<|> PostNammaTagTimeBoundsCreate :<|> DeleteNammaTagTimeBoundsDelete :<|> GetNammaTagAppDynamicLogicGetLogicRollout :<|> PostNammaTagAppDynamicLogicUpsertLogicRollout :<|> GetNammaTagAppDynamicLogicVersions :<|> GetNammaTagAppDynamicLogicDomains :<|> GetNammaTagQueryAll :<|> PostNammaTagUpdateCustomerTag))

type PostNammaTagTagCreate = ("tag" :> "create" :> ReqBody '[JSON] Lib.Yudhishthira.Types.CreateNammaTagRequest :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostNammaTagTagUpdate = ("tag" :> "update" :> ReqBody '[JSON] Lib.Yudhishthira.Types.UpdateNammaTagRequest :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type DeleteNammaTagTagDelete = ("tag" :> "delete" :> MandatoryQueryParam "tagName" Kernel.Prelude.Text :> Delete '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostNammaTagQueryCreate = ("query" :> "create" :> ReqBody '[JSON] Lib.Yudhishthira.Types.ChakraQueriesAPIEntity :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostNammaTagQueryUpdate = ("query" :> "update" :> ReqBody '[JSON] Lib.Yudhishthira.Types.ChakraQueryUpdateReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type DeleteNammaTagQueryDelete = ("query" :> "delete" :> ReqBody '[JSON] Lib.Yudhishthira.Types.ChakraQueryDeleteReq :> Delete '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostNammaTagAppDynamicLogicVerify = ("appDynamicLogic" :> "verify" :> ReqBody '[JSON] Lib.Yudhishthira.Types.AppDynamicLogicReq :> Post '[JSON] Lib.Yudhishthira.Types.AppDynamicLogicResp)

type GetNammaTagAppDynamicLogic =
  ( "appDynamicLogic" :> QueryParam "version" Kernel.Prelude.Int :> MandatoryQueryParam "domain" Lib.Yudhishthira.Types.LogicDomain
      :> Get
           '[JSON]
           [Lib.Yudhishthira.Types.GetLogicsResp]
  )

type PostNammaTagRunJob = ("runJob" :> ReqBody '[JSON] Lib.Yudhishthira.Types.RunKaalChakraJobReq :> Post '[JSON] Lib.Yudhishthira.Types.RunKaalChakraJobRes)

type GetNammaTagTimeBounds = ("timeBounds" :> MandatoryQueryParam "domain" Lib.Yudhishthira.Types.LogicDomain :> Get '[JSON] Lib.Yudhishthira.Types.TimeBoundResp)

type PostNammaTagTimeBoundsCreate = ("timeBounds" :> "create" :> ReqBody '[JSON] Lib.Yudhishthira.Types.CreateTimeBoundRequest :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type DeleteNammaTagTimeBoundsDelete =
  ( "timeBounds" :> "delete" :> MandatoryQueryParam "domain" Lib.Yudhishthira.Types.LogicDomain :> MandatoryQueryParam "name" Kernel.Prelude.Text
      :> Delete
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetNammaTagAppDynamicLogicGetLogicRollout =
  ( "appDynamicLogic" :> "getLogicRollout" :> QueryParam "timeBound" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "domain"
           Lib.Yudhishthira.Types.LogicDomain
      :> Get '[JSON] [Lib.Yudhishthira.Types.LogicRolloutObject]
  )

type PostNammaTagAppDynamicLogicUpsertLogicRollout =
  ( "appDynamicLogic" :> "upsertLogicRollout" :> ReqBody '[JSON] Lib.Yudhishthira.Types.LogicRolloutReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetNammaTagAppDynamicLogicVersions =
  ( "appDynamicLogic" :> "versions" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "domain"
           Lib.Yudhishthira.Types.LogicDomain
      :> Get '[JSON] Lib.Yudhishthira.Types.AppDynamicLogicVersionResp
  )

type GetNammaTagAppDynamicLogicDomains = ("appDynamicLogic" :> "domains" :> Get '[JSON] Lib.Yudhishthira.Types.AppDynamicLogicDomainResp)

type GetNammaTagQueryAll = ("query" :> "all" :> MandatoryQueryParam "chakra" Lib.Yudhishthira.Types.Chakra :> Get '[JSON] Lib.Yudhishthira.Types.ChakraQueryResp)

type PostNammaTagUpdateCustomerTag =
  ( Capture "customerId" (Kernel.Types.Id.Id Dashboard.Common.User) :> "updateCustomerTag" :> ReqBody '[JSON] Lib.Yudhishthira.Types.UpdateTagReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data NammaTagAPIs = NammaTagAPIs
  { postNammaTagTagCreate :: Lib.Yudhishthira.Types.CreateNammaTagRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postNammaTagTagUpdate :: Lib.Yudhishthira.Types.UpdateNammaTagRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteNammaTagTagDelete :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postNammaTagQueryCreate :: Lib.Yudhishthira.Types.ChakraQueriesAPIEntity -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postNammaTagQueryUpdate :: Lib.Yudhishthira.Types.ChakraQueryUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteNammaTagQueryDelete :: Lib.Yudhishthira.Types.ChakraQueryDeleteReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postNammaTagAppDynamicLogicVerify :: Lib.Yudhishthira.Types.AppDynamicLogicReq -> EulerHS.Types.EulerClient Lib.Yudhishthira.Types.AppDynamicLogicResp,
    getNammaTagAppDynamicLogic :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Lib.Yudhishthira.Types.LogicDomain -> EulerHS.Types.EulerClient [Lib.Yudhishthira.Types.GetLogicsResp],
    postNammaTagRunJob :: Lib.Yudhishthira.Types.RunKaalChakraJobReq -> EulerHS.Types.EulerClient Lib.Yudhishthira.Types.RunKaalChakraJobRes,
    getNammaTagTimeBounds :: Lib.Yudhishthira.Types.LogicDomain -> EulerHS.Types.EulerClient Lib.Yudhishthira.Types.TimeBoundResp,
    postNammaTagTimeBoundsCreate :: Lib.Yudhishthira.Types.CreateTimeBoundRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteNammaTagTimeBoundsDelete :: Lib.Yudhishthira.Types.LogicDomain -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getNammaTagAppDynamicLogicGetLogicRollout :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Lib.Yudhishthira.Types.LogicDomain -> EulerHS.Types.EulerClient [Lib.Yudhishthira.Types.LogicRolloutObject],
    postNammaTagAppDynamicLogicUpsertLogicRollout :: Lib.Yudhishthira.Types.LogicRolloutReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getNammaTagAppDynamicLogicVersions :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Lib.Yudhishthira.Types.LogicDomain -> EulerHS.Types.EulerClient Lib.Yudhishthira.Types.AppDynamicLogicVersionResp,
    getNammaTagAppDynamicLogicDomains :: EulerHS.Types.EulerClient Lib.Yudhishthira.Types.AppDynamicLogicDomainResp,
    getNammaTagQueryAll :: Lib.Yudhishthira.Types.Chakra -> EulerHS.Types.EulerClient Lib.Yudhishthira.Types.ChakraQueryResp,
    postNammaTagUpdateCustomerTag :: Kernel.Types.Id.Id Dashboard.Common.User -> Lib.Yudhishthira.Types.UpdateTagReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkNammaTagAPIs :: (Client EulerHS.Types.EulerClient API -> NammaTagAPIs)
mkNammaTagAPIs nammaTagClient = (NammaTagAPIs {..})
  where
    postNammaTagTagCreate :<|> postNammaTagTagUpdate :<|> deleteNammaTagTagDelete :<|> postNammaTagQueryCreate :<|> postNammaTagQueryUpdate :<|> deleteNammaTagQueryDelete :<|> postNammaTagAppDynamicLogicVerify :<|> getNammaTagAppDynamicLogic :<|> postNammaTagRunJob :<|> getNammaTagTimeBounds :<|> postNammaTagTimeBoundsCreate :<|> deleteNammaTagTimeBoundsDelete :<|> getNammaTagAppDynamicLogicGetLogicRollout :<|> postNammaTagAppDynamicLogicUpsertLogicRollout :<|> getNammaTagAppDynamicLogicVersions :<|> getNammaTagAppDynamicLogicDomains :<|> getNammaTagQueryAll :<|> postNammaTagUpdateCustomerTag = nammaTagClient

data NammaTagUserActionType
  = POST_NAMMA_TAG_TAG_CREATE
  | POST_NAMMA_TAG_TAG_UPDATE
  | DELETE_NAMMA_TAG_TAG_DELETE
  | POST_NAMMA_TAG_QUERY_CREATE
  | POST_NAMMA_TAG_QUERY_UPDATE
  | DELETE_NAMMA_TAG_QUERY_DELETE
  | POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_VERIFY
  | GET_NAMMA_TAG_APP_DYNAMIC_LOGIC
  | POST_NAMMA_TAG_RUN_JOB
  | GET_NAMMA_TAG_TIME_BOUNDS
  | POST_NAMMA_TAG_TIME_BOUNDS_CREATE
  | DELETE_NAMMA_TAG_TIME_BOUNDS_DELETE
  | GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_GET_LOGIC_ROLLOUT
  | POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_UPSERT_LOGIC_ROLLOUT
  | GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_VERSIONS
  | GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_DOMAINS
  | GET_NAMMA_TAG_QUERY_ALL
  | POST_NAMMA_TAG_UPDATE_CUSTOMER_TAG
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''NammaTagUserActionType])
