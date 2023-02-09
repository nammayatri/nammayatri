{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module BAPClient.AppBackend
  ( callAppBackendBAP,
    CustomerAPIs (..),
    AppBackendAPIs (..),
  )
where

import qualified "app-backend" API.Dashboard as BAP
import qualified Dashboard.BAP.Merchant as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "app-backend" Domain.Types.Person as BAP
import Domain.Types.ServerName
import qualified EulerHS.Types as Euler
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common hiding (callAPI)
import Servant
import Tools.Auth.Merchant (CheckedShortId)
import Tools.Client

data AppBackendAPIs = AppBackendAPIs
  { customers :: CustomerAPIs,
    merchant :: MerchantAPIs
  }

data CustomerAPIs = CustomerAPIs
  { customerList :: Maybe Integer -> Maybe Integer -> Euler.EulerClient Text,
    customerUpdate :: Id BAP.Person -> Text -> Euler.EulerClient Text
  }

data MerchantAPIs = MerchantAPIs
  { merchantUpdate :: Common.MerchantUpdateReq -> Euler.EulerClient APISuccess,
    mapsServiceConfigUpdate :: Common.MapsServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    mapsServiceUsageConfigUpdate :: Common.MapsServiceUsageConfigUpdateReq -> Euler.EulerClient APISuccess,
    smsServiceConfigUpdate :: Common.SmsServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    smsServiceUsageConfigUpdate :: Common.SmsServiceUsageConfigUpdateReq -> Euler.EulerClient APISuccess
  }

mkAppBackendAPIs :: CheckedShortId DM.Merchant -> Text -> AppBackendAPIs
mkAppBackendAPIs merchantId token = do
  let customers = CustomerAPIs {..}
  let merchant = MerchantAPIs {..}
  AppBackendAPIs {..}
  where
    customersClient
      :<|> merchantClient = clientWithMerchant (Proxy :: Proxy BAP.API') merchantId token

    customerList
      :<|> customerUpdate = customersClient

    merchantUpdate
      :<|> mapsServiceConfigUpdate
      :<|> mapsServiceUsageConfigUpdate
      :<|> smsServiceConfigUpdate
      :<|> smsServiceUsageConfigUpdate = merchantClient

callAppBackendBAP ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI AppBackendAPIs m r b c
  ) =>
  CheckedShortId DM.Merchant ->
  (AppBackendAPIs -> b) ->
  c
callAppBackendBAP merchantId = callServerAPI @_ @m @r APP_BACKEND (mkAppBackendAPIs merchantId) "callAppBackendBAP"
