{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.DynamicOfferDriver.Management.Revenue
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Revenue
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified ProviderPlatformClient.DynamicOfferDriver.Operations
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

type API = ("revenue" :> (GetRevenueCollectionHistory :<|> GetRevenueAllFeeHistory))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getRevenueCollectionHistory merchantId city :<|> getRevenueAllFeeHistory merchantId city

type GetRevenueCollectionHistory = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'VOLUNTEER 'VOLUNTEER_COLLECTION_HISTORY :> API.Types.ProviderPlatform.Management.Revenue.GetRevenueCollectionHistory)

type GetRevenueAllFeeHistory = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'VOLUNTEER 'ALL_FEE_HISTORY :> API.Types.ProviderPlatform.Management.Revenue.GetRevenueAllFeeHistory)

getRevenueCollectionHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Revenue.CollectionList)
getRevenueCollectionHistory merchantShortId opCity apiTokenInfo a1 a2 a3 a4 =
  withFlowHandlerAPI' $
    ( do
        checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
        ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.revenueDSL.getRevenueCollectionHistory) a1 a2 a3 a4
    )

getRevenueAllFeeHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Revenue.AllFees])
getRevenueAllFeeHistory merchantShortId opCity apiTokenInfo a1 a2 =
  withFlowHandlerAPI' $
    ( do
        checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
        ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.revenueDSL.getRevenueAllFeeHistory) a1 a2
    )
