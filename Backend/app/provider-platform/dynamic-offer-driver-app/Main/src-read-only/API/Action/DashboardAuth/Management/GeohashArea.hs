{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.GeohashArea
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.GeohashArea
import qualified Dashboard.Common.GeohashArea
import qualified Domain.Action.Dashboard.Management.GeohashArea
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("geohashArea" :> (GetGeohashAreaList :<|> PostGeohashAreaUpsert :<|> PostGeohashAreaUpsertCsv))

type GetGeohashAreaList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/GEOHASH_AREA/GET_GEOHASH_AREA_LIST"
      :> API.Types.ProviderPlatform.Management.GeohashArea.GetGeohashAreaList
  )

type PostGeohashAreaUpsert =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/GEOHASH_AREA/POST_GEOHASH_AREA_UPSERT"
      :> API.Types.ProviderPlatform.Management.GeohashArea.PostGeohashAreaUpsert
  )

type PostGeohashAreaUpsertCsv =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/GEOHASH_AREA/POST_GEOHASH_AREA_UPSERT_CSV"
      :> API.Types.ProviderPlatform.Management.GeohashArea.PostGeohashAreaUpsertCsv
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getGeohashAreaList merchantId city :<|> postGeohashAreaUpsert merchantId city :<|> postGeohashAreaUpsertCsv merchantId city

getGeohashAreaList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler [Dashboard.Common.GeohashArea.GeohashAreaItem])
getGeohashAreaList a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.GeohashArea.getGeohashAreaList a3 a2

postGeohashAreaUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.GeohashArea.GeohashAreaBulkUpsertReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postGeohashAreaUpsert a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.GeohashArea.postGeohashAreaUpsert a4 a3 a1

postGeohashAreaUpsertCsv :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.GeohashArea.GeohashAreaCsvReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postGeohashAreaUpsertCsv a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.GeohashArea.postGeohashAreaUpsertCsv a4 a3 a1
