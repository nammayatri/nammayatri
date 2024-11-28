{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.PartnerOrgStation
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.PartnerOrgStation
import qualified Domain.Action.RiderPlatform.Management.PartnerOrgStation
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

type API = ("partnerOrgStation" :> (PostPartnerOrgStationCreatePartnerOrgStation :<|> PostPartnerOrgStationUpdatePartnerOrgStation))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postPartnerOrgStationCreatePartnerOrgStation merchantId city :<|> postPartnerOrgStationUpdatePartnerOrgStation merchantId city

type PostPartnerOrgStationCreatePartnerOrgStation =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.PARTNER_ORG_STATION / 'API.Types.RiderPlatform.Management.PartnerOrgStation.POST_PARTNER_ORG_STATION_CREATE_PARTNER_ORG_STATION)
      :> API.Types.RiderPlatform.Management.PartnerOrgStation.PostPartnerOrgStationCreatePartnerOrgStation
  )

type PostPartnerOrgStationUpdatePartnerOrgStation =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.PARTNER_ORG_STATION / 'API.Types.RiderPlatform.Management.PartnerOrgStation.POST_PARTNER_ORG_STATION_UPDATE_PARTNER_ORG_STATION)
      :> API.Types.RiderPlatform.Management.PartnerOrgStation.PostPartnerOrgStationUpdatePartnerOrgStation
  )

postPartnerOrgStationCreatePartnerOrgStation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.PartnerOrgStation.CreatePartnerOrgStationReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPartnerOrgStationCreatePartnerOrgStation merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.PartnerOrgStation.postPartnerOrgStationCreatePartnerOrgStation merchantShortId opCity apiTokenInfo req

postPartnerOrgStationUpdatePartnerOrgStation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.PartnerOrgStation.UpdatePartnerOrgStationReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPartnerOrgStationUpdatePartnerOrgStation merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.PartnerOrgStation.postPartnerOrgStationUpdatePartnerOrgStation merchantShortId opCity apiTokenInfo req
