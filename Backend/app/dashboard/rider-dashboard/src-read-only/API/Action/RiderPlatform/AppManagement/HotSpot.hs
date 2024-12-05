{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.HotSpot
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.HotSpot
import qualified Domain.Action.RiderPlatform.AppManagement.HotSpot
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("hotSpot" :> PostHotSpotRemoveExpired)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postHotSpotRemoveExpired merchantId city

type PostHotSpotRemoveExpired =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.HOT_SPOT / 'API.Types.Dashboard.AppManagement.HotSpot.POST_HOT_SPOT_REMOVE_EXPIRED)
      :> API.Types.Dashboard.AppManagement.HotSpot.PostHotSpotRemoveExpired
  )

postHotSpotRemoveExpired :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postHotSpotRemoveExpired merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.HotSpot.postHotSpotRemoveExpired merchantShortId opCity apiTokenInfo
