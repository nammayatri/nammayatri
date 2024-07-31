{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Driver
  ( API.Types.ProviderPlatform.Management.Driver.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Driver
import qualified Domain.Action.Dashboard.Driver as Domain.Action.Dashboard.Driver
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.Driver.API)
handler merchantId city = getDriverPanAadharSelfieDetails merchantId city :<|> postDriverSyncDocAadharPan merchantId city

getDriverPanAadharSelfieDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Driver.PanAadharSelfieDetailsResp)
getDriverPanAadharSelfieDetails a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Driver.getDriverPanAadharSelfieDetails a4 a3 a2 a1

postDriverSyncDocAadharPan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Driver.AadharPanSyncReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverSyncDocAadharPan a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Driver.postDriverSyncDocAadharPan a3 a2 a1
