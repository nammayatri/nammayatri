{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module BAPClient.AppBackend
  ( callAppBackendBAP,
    CustomerAPIs (..),
    AppBackendAPIs (..),
  )
where

import qualified "app-backend" API.Dashboard as BAP
import Beckn.Prelude
import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Types.Id
import Beckn.Utils.Common hiding (callAPI)
import qualified "lib-dashboard" Domain.Types.Merchant as DMerchant
import qualified "app-backend" Domain.Types.Person as BAP
import Domain.Types.ServerName
import qualified EulerHS.Types as Euler
import Servant
import Servant.Client hiding (client)
import Tools.Auth.Merchant (CheckedShortId)
import Tools.Client

newtype AppBackendAPIs = AppBackendAPIs
  { customers :: CustomerAPIs
  }

data CustomerAPIs = CustomerAPIs
  { customerList :: Maybe Integer -> Maybe Integer -> Euler.EulerClient Text,
    customerUpdate :: Id BAP.Person -> Text -> Euler.EulerClient Text
  }

mkAppBackendAPIs :: CheckedShortId DMerchant.Merchant -> Text -> AppBackendAPIs
mkAppBackendAPIs merchantId token = do
  let customers = CustomerAPIs {..}
  AppBackendAPIs {..}
  where
    customerList
      :<|> customerUpdate = client (Proxy :: Proxy BAP.API') merchantId token

type ApiWithMerchant api =
  Capture "merchantId" (CheckedShortId DMerchant.Merchant)
    :> api

client ::
  forall api.
  HasClient Euler.EulerClient api =>
  Proxy api ->
  Client Euler.EulerClient (ApiWithMerchant api)
client _ = Euler.client (Proxy @(ApiWithMerchant api))

callAppBackendBAP ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI AppBackendAPIs m r b c
  ) =>
  CheckedShortId DMerchant.Merchant ->
  (AppBackendAPIs -> b) ->
  c
callAppBackendBAP merchantId = callServerAPI @_ @m @r APP_BACKEND (mkAppBackendAPIs merchantId) "callAppBackendBAP"
