{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.MultiModal
  ( API.Types.RiderPlatform.Management.MultiModal.API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.MultiModal
import qualified Domain.Action.Dashboard.MultiModal as Domain.Action.Dashboard.MultiModal
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.MultiModal.API)
handler merchantId city = postMultiModalMultimodalFrfsDataPreprocess merchantId city :<|> postMultiModalMultimodalFrfsDataStatus merchantId city :<|> postMultiModalMultimodalFrfsDataVersionIsReady merchantId city :<|> postMultiModalMultimodalFrfsDataVersionApply merchantId city

postMultiModalMultimodalFrfsDataPreprocess :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.RiderPlatform.Management.MultiModal.PreprocessFRFSDataResp)
postMultiModalMultimodalFrfsDataPreprocess a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.MultiModal.postMultiModalMultimodalFrfsDataPreprocess a2 a1

postMultiModalMultimodalFrfsDataStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.RiderPlatform.Management.MultiModal.FRFSDataStatusResp)
postMultiModalMultimodalFrfsDataStatus a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.MultiModal.postMultiModalMultimodalFrfsDataStatus a2 a1

postMultiModalMultimodalFrfsDataVersionIsReady :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.RiderPlatform.Management.MultiModal.ReadyVersionsResp)
postMultiModalMultimodalFrfsDataVersionIsReady a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.MultiModal.postMultiModalMultimodalFrfsDataVersionIsReady a2 a1

postMultiModalMultimodalFrfsDataVersionApply :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMultiModalMultimodalFrfsDataVersionApply a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.MultiModal.postMultiModalMultimodalFrfsDataVersionApply a2 a1
