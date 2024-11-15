{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.InsertPartnerOrgStation
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.InsertPartnerOrgStation
import qualified Domain.Action.RiderPlatform.Management.InsertPartnerOrgStation
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

type API = ("insertPartnerOrgStation" :> PostInsertPartnerOrgStationInsertPartnerOrgStation)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postInsertPartnerOrgStationInsertPartnerOrgStation merchantId city

type PostInsertPartnerOrgStationInsertPartnerOrgStation =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.INSERT_PARTNER_ORG_STATION) / ('API.Types.RiderPlatform.Management.InsertPartnerOrgStation.POST_INSERT_PARTNER_ORG_STATION_INSERT_PARTNER_ORG_STATION))
      :> API.Types.RiderPlatform.Management.InsertPartnerOrgStation.PostInsertPartnerOrgStationInsertPartnerOrgStation
  )

postInsertPartnerOrgStationInsertPartnerOrgStation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.InsertPartnerOrgStation.InsertData -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postInsertPartnerOrgStationInsertPartnerOrgStation merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.InsertPartnerOrgStation.postInsertPartnerOrgStationInsertPartnerOrgStation merchantShortId opCity apiTokenInfo req
