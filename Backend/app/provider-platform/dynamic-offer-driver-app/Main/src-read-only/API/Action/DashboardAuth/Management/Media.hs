{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Media
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Media
import qualified Domain.Action.Dashboard.Management.Media
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified "shared-services" IssueManagement.Domain.Types.MediaFile
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("media" :> (GetMediaMediaImage))

type GetMediaMediaImage = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/MEDIA/GET_MEDIA_MEDIA_IMAGE" :> API.Types.ProviderPlatform.Management.Media.GetMediaMediaImage)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getMediaMediaImage merchantId city

getMediaMediaImage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Media.GetImageResponse)
getMediaMediaImage a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Media.getMediaMediaImage a4 a3 a1
