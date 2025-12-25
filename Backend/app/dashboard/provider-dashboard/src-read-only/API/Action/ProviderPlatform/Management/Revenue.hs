{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.Revenue
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.Revenue
import qualified Domain.Action.ProviderPlatform.Management.Revenue
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("revenue" :> (GetRevenueCollectionHistory :<|> GetRevenueAllFeeHistory))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getRevenueCollectionHistory merchantId city :<|> getRevenueAllFeeHistory merchantId city

type GetRevenueCollectionHistory =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.REVENUE / 'API.Types.ProviderPlatform.Management.Revenue.GET_REVENUE_COLLECTION_HISTORY)
      :> API.Types.ProviderPlatform.Management.Revenue.GetRevenueCollectionHistory
  )

type GetRevenueAllFeeHistory =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.REVENUE / 'API.Types.ProviderPlatform.Management.Revenue.GET_REVENUE_ALL_FEE_HISTORY)
      :> API.Types.ProviderPlatform.Management.Revenue.GetRevenueAllFeeHistory
  )

getRevenueCollectionHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Revenue.CollectionList)
getRevenueCollectionHistory merchantShortId opCity apiTokenInfo from place to volunteerId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Revenue.getRevenueCollectionHistory merchantShortId opCity apiTokenInfo from place to volunteerId

getRevenueAllFeeHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Revenue.AllFees])
getRevenueAllFeeHistory merchantShortId opCity apiTokenInfo from to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Revenue.getRevenueAllFeeHistory merchantShortId opCity apiTokenInfo from to
