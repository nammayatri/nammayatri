{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.SosMedia
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.SosMedia
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.Management.SosMedia
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("sos-media" :> GetSosMediaSosMedia)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getSosMediaSosMedia merchantId city

type GetSosMediaSosMedia =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.SOS_MEDIA) / ('API.Types.ProviderPlatform.Management.SosMedia.GET_SOS_MEDIA_SOS_MEDIA))
      :> API.Types.ProviderPlatform.Management.SosMedia.GetSosMediaSosMedia
  )

getSosMediaSosMedia :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.SosMedia.GetSosMediaResponse])
getSosMediaSosMedia merchantShortId opCity apiTokenInfo personId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.SosMedia.getSosMediaSosMedia merchantShortId opCity apiTokenInfo personId
