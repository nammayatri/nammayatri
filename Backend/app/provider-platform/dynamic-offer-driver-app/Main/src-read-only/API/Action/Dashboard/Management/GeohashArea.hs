{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.GeohashArea
  ( API.Types.ProviderPlatform.Management.GeohashArea.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.GeohashArea.API)
handler merchantId city = getGeohashAreaList merchantId city :<|> postGeohashAreaUpsert merchantId city :<|> postGeohashAreaUpsertCsv merchantId city

getGeohashAreaList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler [Dashboard.Common.GeohashArea.GeohashAreaItem])
getGeohashAreaList a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.GeohashArea.getGeohashAreaList a2 a1

postGeohashAreaUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.GeohashArea.GeohashAreaBulkUpsertReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postGeohashAreaUpsert a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.GeohashArea.postGeohashAreaUpsert a3 a2 a1

postGeohashAreaUpsertCsv :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.GeohashArea.GeohashAreaCsvReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postGeohashAreaUpsertCsv a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.GeohashArea.postGeohashAreaUpsertCsv a3 a2 a1
