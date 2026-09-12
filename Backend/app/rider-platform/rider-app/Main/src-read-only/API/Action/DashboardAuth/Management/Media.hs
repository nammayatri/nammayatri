{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Media
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Media
import qualified Data.Text
import qualified Domain.Action.Dashboard.Media
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("media" :> (GetMediaFile))

type GetMediaFile = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/MEDIA/GET_MEDIA_FILE" :> API.Types.RiderPlatform.Management.Media.GetMediaFile)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getMediaFile merchantId city

getMediaFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Data.Text.Text -> Environment.FlowHandler API.Types.RiderPlatform.Management.Media.GetMediaFileResponse)
getMediaFile a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Media.getMediaFile a4 a3 a1
