{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.DriverCoins
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.DriverCoins
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.Management.DriverCoins
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

type API = ("coins" :> (PostDriverCoinsBulkUploadCoins :<|> PostDriverCoinsBulkUploadCoinsV2 :<|> GetDriverCoinsCoinHistory :<|> PostDriverCoinsBlacklistedEventsUpdate))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postDriverCoinsBulkUploadCoins merchantId city :<|> postDriverCoinsBulkUploadCoinsV2 merchantId city :<|> getDriverCoinsCoinHistory merchantId city :<|> postDriverCoinsBlacklistedEventsUpdate merchantId city

type PostDriverCoinsBulkUploadCoins =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_COINS / 'API.Types.ProviderPlatform.Management.DriverCoins.POST_DRIVER_COINS_BULK_UPLOAD_COINS)
      :> API.Types.ProviderPlatform.Management.DriverCoins.PostDriverCoinsBulkUploadCoins
  )

type PostDriverCoinsBulkUploadCoinsV2 =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_COINS / 'API.Types.ProviderPlatform.Management.DriverCoins.POST_DRIVER_COINS_BULK_UPLOAD_COINS_V2)
      :> API.Types.ProviderPlatform.Management.DriverCoins.PostDriverCoinsBulkUploadCoinsV2
  )

type GetDriverCoinsCoinHistory =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_COINS / 'API.Types.ProviderPlatform.Management.DriverCoins.GET_DRIVER_COINS_COIN_HISTORY)
      :> API.Types.ProviderPlatform.Management.DriverCoins.GetDriverCoinsCoinHistory
  )

type PostDriverCoinsBlacklistedEventsUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_COINS / 'API.Types.ProviderPlatform.Management.DriverCoins.POST_DRIVER_COINS_BLACKLISTED_EVENTS_UPDATE)
      :> API.Types.ProviderPlatform.Management.DriverCoins.PostDriverCoinsBlacklistedEventsUpdate
  )

postDriverCoinsBulkUploadCoins :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.DriverCoins.BulkUploadCoinsReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverCoins.BulkUploadCoinRes)
postDriverCoinsBulkUploadCoins merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverCoins.postDriverCoinsBulkUploadCoins merchantShortId opCity apiTokenInfo req

postDriverCoinsBulkUploadCoinsV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.DriverCoins.BulkUploadCoinsReqV2 -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverCoins.BulkUploadCoinRes)
postDriverCoinsBulkUploadCoinsV2 merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverCoins.postDriverCoinsBulkUploadCoinsV2 merchantShortId opCity apiTokenInfo req

getDriverCoinsCoinHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Integer -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverCoins.CoinHistoryRes)
getDriverCoinsCoinHistory merchantShortId opCity apiTokenInfo driverId limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverCoins.getDriverCoinsCoinHistory merchantShortId opCity apiTokenInfo driverId limit offset

postDriverCoinsBlacklistedEventsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverCoins.UpdateBlacklistedCoinEventsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverCoinsBlacklistedEventsUpdate merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverCoins.postDriverCoinsBlacklistedEventsUpdate merchantShortId opCity apiTokenInfo driverId req
