{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.Media
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Media
import qualified Data.Text
import qualified Domain.Action.RiderPlatform.Management.Media
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("media" :> GetMediaFile)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getMediaFile merchantId city

type GetMediaFile =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.MEDIA) / ('API.Types.RiderPlatform.Management.Media.GET_MEDIA_FILE))
      :> API.Types.RiderPlatform.Management.Media.GetMediaFile
  )

getMediaFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Text.Text -> Environment.FlowHandler API.Types.RiderPlatform.Management.Media.GetMediaFileResponse)
getMediaFile merchantShortId opCity apiTokenInfo filePath = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Media.getMediaFile merchantShortId opCity apiTokenInfo filePath
