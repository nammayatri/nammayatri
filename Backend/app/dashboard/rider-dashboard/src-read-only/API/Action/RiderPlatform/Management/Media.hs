{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.RiderPlatform.Management.Media 
( API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth.Api
import Kernel.Utils.Common
import Storage.Beam.CommonInstances ()
import qualified API.Types.RiderPlatform.Management.Media
import qualified API.Types.RiderPlatform.Management
import qualified Domain.Action.RiderPlatform.Management.Media
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified Data.Text
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment


type API = ("media" :> GetMediaFile)
handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getMediaFile merchantId city
type GetMediaFile = (ApiAuth ('APP_BACKEND_MANAGEMENT)
                             ('DSL)
                             (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.MEDIA) / ('API.Types.RiderPlatform.Management.Media.GET_MEDIA_FILE)) :> API.Types.RiderPlatform.Management.Media.GetMediaFile)
getMediaFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Text.Text -> Environment.FlowHandler API.Types.RiderPlatform.Management.Media.GetMediaFileResponse)
getMediaFile merchantShortId opCity apiTokenInfo filePath = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Media.getMediaFile merchantShortId opCity apiTokenInfo filePath



