{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.GeohashArea
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.GeohashArea
import qualified Dashboard.Common.GeohashArea
import qualified Domain.Action.ProviderPlatform.Management.GeohashArea
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("geohashArea" :> (GetGeohashAreaList :<|> PostGeohashAreaUpsert :<|> PostGeohashAreaUpsertCsv))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getGeohashAreaList merchantId city :<|> postGeohashAreaUpsert merchantId city :<|> postGeohashAreaUpsertCsv merchantId city

type GetGeohashAreaList =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.GEOHASH_AREA) / ('API.Types.ProviderPlatform.Management.GeohashArea.GET_GEOHASH_AREA_LIST))
      :> API.Types.ProviderPlatform.Management.GeohashArea.GetGeohashAreaList
  )

type PostGeohashAreaUpsert =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.GEOHASH_AREA) / ('API.Types.ProviderPlatform.Management.GeohashArea.GEOHASH_AREA_BULK_UPSERT))
      :> API.Types.ProviderPlatform.Management.GeohashArea.PostGeohashAreaUpsert
  )

type PostGeohashAreaUpsertCsv =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.GEOHASH_AREA) / ('API.Types.ProviderPlatform.Management.GeohashArea.GEOHASH_AREA_CSV_UPSERT))
      :> API.Types.ProviderPlatform.Management.GeohashArea.PostGeohashAreaUpsertCsv
  )

getGeohashAreaList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler [Dashboard.Common.GeohashArea.GeohashAreaItem])
getGeohashAreaList merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.GeohashArea.getGeohashAreaList merchantShortId opCity apiTokenInfo

postGeohashAreaUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.GeohashArea.GeohashAreaBulkUpsertReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postGeohashAreaUpsert merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.GeohashArea.postGeohashAreaUpsert merchantShortId opCity apiTokenInfo req

postGeohashAreaUpsertCsv :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.GeohashArea.GeohashAreaCsvReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postGeohashAreaUpsertCsv merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.GeohashArea.postGeohashAreaUpsertCsv merchantShortId opCity apiTokenInfo req
