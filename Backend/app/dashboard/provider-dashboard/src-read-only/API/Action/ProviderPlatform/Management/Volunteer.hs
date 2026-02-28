{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.Volunteer
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.Volunteer
import qualified Domain.Action.ProviderPlatform.Management.Volunteer
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("volunteer" :> (PostVolunteerCreate :<|> GetVolunteerList :<|> PostVolunteerUpdate))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postVolunteerCreate merchantId city :<|> getVolunteerList merchantId city :<|> postVolunteerUpdate merchantId city

type PostVolunteerCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.VOLUNTEER / 'API.Types.ProviderPlatform.Management.Volunteer.POST_VOLUNTEER_CREATE)
      :> API.Types.ProviderPlatform.Management.Volunteer.PostVolunteerCreate
  )

type GetVolunteerList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.VOLUNTEER / 'API.Types.ProviderPlatform.Management.Volunteer.GET_VOLUNTEER_LIST)
      :> API.Types.ProviderPlatform.Management.Volunteer.GetVolunteerList
  )

type PostVolunteerUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.VOLUNTEER / 'API.Types.ProviderPlatform.Management.Volunteer.POST_VOLUNTEER_UPDATE)
      :> API.Types.ProviderPlatform.Management.Volunteer.PostVolunteerUpdate
  )

postVolunteerCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Volunteer.CreateVolunteerReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Volunteer.CreateVolunteerRes)
postVolunteerCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Volunteer.postVolunteerCreate merchantShortId opCity apiTokenInfo req

getVolunteerList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Volunteer.VolunteerListRes)
getVolunteerList merchantShortId opCity apiTokenInfo limit offset volunteerId vendorId isActive place = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Volunteer.getVolunteerList merchantShortId opCity apiTokenInfo limit offset volunteerId vendorId isActive place

postVolunteerUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.Volunteer.UpdateVolunteerReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postVolunteerUpdate merchantShortId opCity apiTokenInfo volunteerId vendorId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Volunteer.postVolunteerUpdate merchantShortId opCity apiTokenInfo volunteerId vendorId req
