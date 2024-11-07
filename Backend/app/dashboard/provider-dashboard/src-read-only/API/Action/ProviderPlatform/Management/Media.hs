{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.Media
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.Media
import qualified Domain.Action.ProviderPlatform.Management.Media
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified "shared-services" IssueManagement.Domain.Types.MediaFile
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("media" :> GetMediaMediaImage)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getMediaMediaImage merchantId city

type GetMediaMediaImage =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.MEDIA / 'API.Types.ProviderPlatform.Management.Media.GET_MEDIA_MEDIA_IMAGE)
      :> API.Types.ProviderPlatform.Management.Media.GetMediaMediaImage
  )

getMediaMediaImage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Media.GetImageResponse)
getMediaMediaImage merchantShortId opCity apiTokenInfo imageId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Media.getMediaMediaImage merchantShortId opCity apiTokenInfo imageId
