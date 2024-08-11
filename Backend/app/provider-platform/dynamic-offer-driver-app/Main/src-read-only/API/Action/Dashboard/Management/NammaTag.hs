{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.NammaTag
  ( API.Types.ProviderPlatform.Management.NammaTag.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.NammaTag
import qualified Domain.Action.Dashboard.Management.NammaTag as Domain.Action.Dashboard.Management.NammaTag
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.NammaTag.API)
handler merchantId city = postNammaTagTagCreate merchantId city :<|> postNammaTagQueryCreate merchantId city

postNammaTagTagCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.CreateNammaTagRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagTagCreate a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagTagCreate a3 a2 a1

postNammaTagQueryCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.ChakraQueriesAPIEntity -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagQueryCreate a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagQueryCreate a3 a2 a1
