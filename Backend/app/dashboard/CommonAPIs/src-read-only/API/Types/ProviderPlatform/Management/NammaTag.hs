{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.NammaTag where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Lib.Yudhishthira.Types
import Servant
import Servant.Client

type API = ("nammaTag" :> (PostNammaTagTagCreate :<|> PostNammaTagQueryCreate :<|> PostNammaTagAppDynamicLogicVerify))

type PostNammaTagTagCreate = ("tag" :> "create" :> ReqBody '[JSON] Lib.Yudhishthira.Types.CreateNammaTagRequest :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostNammaTagQueryCreate = ("query" :> "create" :> ReqBody '[JSON] Lib.Yudhishthira.Types.ChakraQueriesAPIEntity :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostNammaTagAppDynamicLogicVerify = ("appDynamicLogic" :> "verify" :> ReqBody '[JSON] Lib.Yudhishthira.Types.AppDynamicLogicReq :> Post '[JSON] Lib.Yudhishthira.Types.AppDynamicLogicResp)

data NammaTagAPIs = NammaTagAPIs
  { postNammaTagTagCreate :: Lib.Yudhishthira.Types.CreateNammaTagRequest -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postNammaTagQueryCreate :: Lib.Yudhishthira.Types.ChakraQueriesAPIEntity -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postNammaTagAppDynamicLogicVerify :: Lib.Yudhishthira.Types.AppDynamicLogicReq -> EulerHS.Types.EulerClient Lib.Yudhishthira.Types.AppDynamicLogicResp
  }

mkNammaTagAPIs :: (Client EulerHS.Types.EulerClient API -> NammaTagAPIs)
mkNammaTagAPIs nammaTagClient = (NammaTagAPIs {..})
  where
    postNammaTagTagCreate :<|> postNammaTagQueryCreate :<|> postNammaTagAppDynamicLogicVerify = nammaTagClient
