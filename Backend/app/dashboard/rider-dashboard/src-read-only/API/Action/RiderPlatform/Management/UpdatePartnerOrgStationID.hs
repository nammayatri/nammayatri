{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.UpdatePartnerOrgStationID
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.UpdatePartnerOrgStationID
import qualified Domain.Action.RiderPlatform.Management.UpdatePartnerOrgStationID
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

type API = ("updatePartnerOrgStationID" :> PostUpdatePartnerOrgStationIDUpdatePartnerOrgStationID)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postUpdatePartnerOrgStationIDUpdatePartnerOrgStationID merchantId city

type PostUpdatePartnerOrgStationIDUpdatePartnerOrgStationID =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.UPDATE_PARTNER_ORG_STATION_ID / 'API.Types.RiderPlatform.Management.UpdatePartnerOrgStationID.POST_UPDATE_PARTNER_ORG_STATION_ID_UPDATE_PARTNER_ORG_STATION_ID)
      :> API.Types.RiderPlatform.Management.UpdatePartnerOrgStationID.PostUpdatePartnerOrgStationIDUpdatePartnerOrgStationID
  )

postUpdatePartnerOrgStationIDUpdatePartnerOrgStationID :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.UpdatePartnerOrgStationID.ReqData -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postUpdatePartnerOrgStationIDUpdatePartnerOrgStationID merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.UpdatePartnerOrgStationID.postUpdatePartnerOrgStationIDUpdatePartnerOrgStationID merchantShortId opCity apiTokenInfo req
