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
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.NammaTag.API)
handler merchantId city = postNammaTagTagCreate merchantId city :<|> postNammaTagTagUpdate merchantId city :<|> deleteNammaTagTagDelete merchantId city :<|> postNammaTagQueryCreate merchantId city :<|> postNammaTagAppDynamicLogicVerify merchantId city :<|> getNammaTagAppDynamicLogic merchantId city :<|> postNammaTagRunJob merchantId city :<|> postNammaTagTimeBoundsCreate merchantId city :<|> deleteNammaTagTimeBoundsDelete merchantId city :<|> getNammaTagAppDynamicLogicGetLogicRollout merchantId city :<|> postNammaTagAppDynamicLogicUpsertLogicRollout merchantId city

postNammaTagTagCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.CreateNammaTagRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagTagCreate a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagTagCreate a3 a2 a1

postNammaTagTagUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.UpdateNammaTagRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagTagUpdate a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagTagUpdate a3 a2 a1

deleteNammaTagTagDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteNammaTagTagDelete a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.deleteNammaTagTagDelete a3 a2 a1

postNammaTagQueryCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.ChakraQueriesAPIEntity -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagQueryCreate a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagQueryCreate a3 a2 a1

postNammaTagAppDynamicLogicVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.AppDynamicLogicReq -> Environment.FlowHandler Lib.Yudhishthira.Types.AppDynamicLogicResp)
postNammaTagAppDynamicLogicVerify a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagAppDynamicLogicVerify a3 a2 a1

getNammaTagAppDynamicLogic :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler [Lib.Yudhishthira.Types.GetLogicsResp])
getNammaTagAppDynamicLogic a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagAppDynamicLogic a4 a3 a2 a1

postNammaTagRunJob :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.RunKaalChakraJobReq -> Environment.FlowHandler Lib.Yudhishthira.Types.RunKaalChakraJobRes)
postNammaTagRunJob a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagRunJob a3 a2 a1

postNammaTagTimeBoundsCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.CreateTimeBoundRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagTimeBoundsCreate a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagTimeBoundsCreate a3 a2 a1

deleteNammaTagTimeBoundsDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.LogicDomain -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteNammaTagTimeBoundsDelete a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.deleteNammaTagTimeBoundsDelete a4 a3 a2 a1

getNammaTagAppDynamicLogicGetLogicRollout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Lib.Yudhishthira.Types.LogicDomain -> Environment.FlowHandler [Lib.Yudhishthira.Types.LogicRolloutObject])
getNammaTagAppDynamicLogicGetLogicRollout a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.getNammaTagAppDynamicLogicGetLogicRollout a4 a3 a2 a1

postNammaTagAppDynamicLogicUpsertLogicRollout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.LogicRolloutReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postNammaTagAppDynamicLogicUpsertLogicRollout a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Management.NammaTag.postNammaTagAppDynamicLogicUpsertLogicRollout a3 a2 a1
