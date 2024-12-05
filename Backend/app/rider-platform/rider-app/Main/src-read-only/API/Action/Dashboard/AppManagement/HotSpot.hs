{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.HotSpot
  ( API.Types.Dashboard.AppManagement.HotSpot.API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.HotSpot
import qualified Domain.Action.Dashboard.AppManagement.HotSpot as Domain.Action.Dashboard.AppManagement.HotSpot
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.HotSpot.API)
handler merchantId city = postHotSpotRemoveExpired merchantId city

postHotSpotRemoveExpired :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postHotSpotRemoveExpired a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.HotSpot.postHotSpotRemoveExpired a2 a1
