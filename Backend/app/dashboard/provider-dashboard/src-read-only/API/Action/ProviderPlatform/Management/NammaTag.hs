{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.NammaTag
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.NammaTag
import qualified Domain.Action.ProviderPlatform.Management.NammaTag
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogic
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("nammaTag" :> (PostNammaTagTagCreate :<|> PostNammaTagQueryCreate :<|> PostNammaTagAppDynamicLogicVerify :<|> GetNammaTagAppDynamicLogic :<|> PostNammaTagRunJob))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postNammaTagTagCreate merchantId city :<|> postNammaTagQueryCreate merchantId city :<|> postNammaTagAppDynamicLogicVerify merchantId city :<|> getNammaTagAppDynamicLogic merchantId city :<|> postNammaTagRunJob merchantId city

type PostNammaTagTagCreate = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'NAMMA_TAG 'CREATE_NAMMA_TAG :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagTagCreate)

type PostNammaTagQueryCreate = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'NAMMA_TAG 'CREATE_CHAKRA_QUERY :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagQueryCreate)

type PostNammaTagAppDynamicLogicVerify =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'NAMMA_TAG
      'APP_DYNAMIC_LOGIC_VERIFY
      :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagAppDynamicLogicVerify
  )

type GetNammaTagAppDynamicLogic = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'NAMMA_TAG 'APP_DYNAMIC_LOGIC_VERIFY :> API.Types.ProviderPlatform.Management.NammaTag.GetNammaTagAppDynamicLogic)

type PostNammaTagRunJob = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'NAMMA_TAG 'RUN_KAAL_CHAKRA_JOB :> API.Types.ProviderPlatform.Management.NammaTag.PostNammaTagRunJob)

postNammaTagTagCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.CreateNammaTagRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagTagCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.postNammaTagTagCreate merchantShortId opCity apiTokenInfo req

postNammaTagQueryCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.ChakraQueriesAPIEntity -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagQueryCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.postNammaTagQueryCreate merchantShortId opCity apiTokenInfo req

postNammaTagAppDynamicLogicVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.AppDynamicLogicReq -> Environment.FlowHandler Lib.Yudhishthira.Types.AppDynamicLogicResp)
postNammaTagAppDynamicLogicVerify merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.postNammaTagAppDynamicLogicVerify merchantShortId opCity apiTokenInfo req

getNammaTagAppDynamicLogic :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler [Lib.Yudhishthira.Types.AppDynamicLogic.AppDynamicLogic])
getNammaTagAppDynamicLogic merchantShortId opCity apiTokenInfo domain = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.getNammaTagAppDynamicLogic merchantShortId opCity apiTokenInfo domain

postNammaTagRunJob :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Lib.Yudhishthira.Types.RunKaalChakraJobReq -> Environment.FlowHandler Lib.Yudhishthira.Types.RunKaalChakraJobRes)
postNammaTagRunJob merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.NammaTag.postNammaTagRunJob merchantShortId opCity apiTokenInfo req
