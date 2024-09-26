{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.NammaTag where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogic
import Servant
import Servant.Client

type API = ("nammaTag" :> (PostNammaTagTagCreate :<|> PostNammaTagTagUpdate :<|> DeleteNammaTagTagDelete :<|> PostNammaTagQueryCreate :<|> PostNammaTagAppDynamicLogicVerify :<|> GetNammaTagAppDynamicLogic :<|> PostNammaTagRunJob))

type PostNammaTagTagCreate = ("tag" :> "create" :> ReqBody '[JSON] Lib.Yudhishthira.Types.CreateNammaTagRequest :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostNammaTagTagUpdate = ("tag" :> "update" :> ReqBody '[JSON] Lib.Yudhishthira.Types.UpdateNammaTagRequest :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type DeleteNammaTagTagDelete = ("tag" :> "delete" :> MandatoryQueryParam "tagName" Kernel.Prelude.Text :> Delete '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostNammaTagQueryCreate = ("query" :> "create" :> ReqBody '[JSON] Lib.Yudhishthira.Types.ChakraQueriesAPIEntity :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostNammaTagAppDynamicLogicVerify = ("appDynamicLogic" :> "verify" :> ReqBody '[JSON] Lib.Yudhishthira.Types.AppDynamicLogicReq :> Post '[JSON] Lib.Yudhishthira.Types.AppDynamicLogicResp)

type GetNammaTagAppDynamicLogic = ("appDynamicLogic" :> MandatoryQueryParam "domain" Lib.Yudhishthira.Types.LogicDomain :> Get '[JSON] [Lib.Yudhishthira.Types.AppDynamicLogic.AppDynamicLogic])

type PostNammaTagRunJob = ("runJob" :> ReqBody '[JSON] Lib.Yudhishthira.Types.RunKaalChakraJobReq :> Post '[JSON] Lib.Yudhishthira.Types.RunKaalChakraJobRes)

data NammaTagAPIs = NammaTagAPIs
  { postNammaTagTagCreate :: Lib.Yudhishthira.Types.CreateNammaTagRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postNammaTagTagUpdate :: Lib.Yudhishthira.Types.UpdateNammaTagRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteNammaTagTagDelete :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postNammaTagQueryCreate :: Lib.Yudhishthira.Types.ChakraQueriesAPIEntity -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postNammaTagAppDynamicLogicVerify :: Lib.Yudhishthira.Types.AppDynamicLogicReq -> EulerHS.Types.EulerClient Lib.Yudhishthira.Types.AppDynamicLogicResp,
    getNammaTagAppDynamicLogic :: Lib.Yudhishthira.Types.LogicDomain -> EulerHS.Types.EulerClient [Lib.Yudhishthira.Types.AppDynamicLogic.AppDynamicLogic],
    postNammaTagRunJob :: Lib.Yudhishthira.Types.RunKaalChakraJobReq -> EulerHS.Types.EulerClient Lib.Yudhishthira.Types.RunKaalChakraJobRes
  }

mkNammaTagAPIs :: (Client EulerHS.Types.EulerClient API -> NammaTagAPIs)
mkNammaTagAPIs nammaTagClient = (NammaTagAPIs {..})
  where
    postNammaTagTagCreate :<|> postNammaTagTagUpdate :<|> deleteNammaTagTagDelete :<|> postNammaTagQueryCreate :<|> postNammaTagAppDynamicLogicVerify :<|> getNammaTagAppDynamicLogic :<|> postNammaTagRunJob = nammaTagClient

data NammaTagEndpointDSL
  = PostNammaTagTagCreateEndpoint
  | PostNammaTagTagUpdateEndpoint
  | DeleteNammaTagTagDeleteEndpoint
  | PostNammaTagQueryCreateEndpoint
  | PostNammaTagAppDynamicLogicVerifyEndpoint
  | GetNammaTagAppDynamicLogicEndpoint
  | PostNammaTagRunJobEndpoint
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
