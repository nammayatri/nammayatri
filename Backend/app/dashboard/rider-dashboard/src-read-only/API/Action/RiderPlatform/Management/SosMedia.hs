{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.RiderPlatform.Management.SosMedia 
( API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth.Api
import Kernel.Utils.Common
import Storage.Beam.CommonInstances ()
import qualified API.Types.RiderPlatform.Management.SosMedia
import qualified API.Types.RiderPlatform.Management
import qualified Domain.Action.RiderPlatform.Management.SosMedia
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified Dashboard.Common
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment


type API = ("sos-media" :> GetSosMediaSosMedia)
handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getSosMediaSosMedia merchantId city
type GetSosMediaSosMedia = (ApiAuth ('APP_BACKEND_MANAGEMENT)
                                    ('DSL)
                                    (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.SOS_MEDIA) / ('API.Types.RiderPlatform.Management.SosMedia.GET_SOS_MEDIA_SOS_MEDIA)) :> API.Types.RiderPlatform.Management.SosMedia.GetSosMediaSosMedia)
getSosMediaSosMedia :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Environment.FlowHandler [API.Types.RiderPlatform.Management.SosMedia.GetSosMediaResponse])
getSosMediaSosMedia merchantShortId opCity apiTokenInfo personId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.SosMedia.getSosMediaSosMedia merchantShortId opCity apiTokenInfo personId



