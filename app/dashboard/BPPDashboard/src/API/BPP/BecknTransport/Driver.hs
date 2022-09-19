{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module API.BPP.BecknTransport.Driver
  ( API,
    handler,
  )
where

import qualified "beckn-transport" API.Dashboard.Driver as BecknTransport
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver as Common
import "lib-dashboard" Domain.Types.Person as DP
import "lib-dashboard" Environment
import qualified EulerHS.Types as T
import GHC.TypeLits
import Servant
import "lib-dashboard" Tools.Auth
import qualified Tools.Client as Client

type API =
  DriverListAPI
    :<|> DriverActivityAPI
    :<|> EnableDriversAPI
    :<|> DisableDriversAPI
    :<|> DriverLocationAPI

handler :: FlowServer API
handler =
  listDriver
    :<|> driverActivity
    :<|> enableDrivers
    :<|> disableDrivers
    :<|> driverLocation

type FromCommon (s :: Symbol) (access :: ApiAccessType) api =
  "driver"
    :> s
    :> ApiAuth access 'DRIVERS
    :> api

type DriverListAPI = FromCommon "list" 'READ_ACCESS Common.DriverListAPI

type DriverActivityAPI = FromCommon "activity" 'READ_ACCESS Common.DriverActivityAPI

type EnableDriversAPI = FromCommon "enable" 'WRITE_ACCESS Common.EnableDriversAPI

type DisableDriversAPI = FromCommon "disable" 'WRITE_ACCESS Common.DisableDriversAPI

type DriverLocationAPI = FromCommon "location" 'READ_ACCESS Common.DriverLocationAPI

listDriver :: Id DP.Person -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> FlowHandler Common.DriverListRes
listDriver _ mbLimit mbOffset verified rejected pendingdoc phone = withFlowHandlerAPI $ do
  -- FIXME: drivers for only one organization?
  Client.callBecknTransportApi (\tok -> client tok mbLimit mbOffset verified rejected pendingdoc phone) "listDriver"
  where
    client =
      T.client
        (Proxy @BecknTransport.DriverListAPI)

driverActivity :: Id DP.Person -> FlowHandler Common.DriverActivityRes
driverActivity _ = withFlowHandlerAPI $ do
  -- FIXME: drivers for only one organization?
  Client.callBecknTransportApi client "driverActivity"
  where
    client =
      T.client (Proxy @BecknTransport.DriverActivityAPI)

enableDrivers :: Id DP.Person -> Common.DriverIds -> FlowHandler Common.EnableDriversRes
enableDrivers _ req = withFlowHandlerAPI $ do
  -- FIXME: drivers for only one organization?
  Client.callBecknTransportApi (\tok -> client tok req) "enableDrivers"
  where
    client =
      T.client (Proxy @BecknTransport.EnableDriversAPI)

disableDrivers :: Id DP.Person -> Common.DriverIds -> FlowHandler Common.DisableDriversRes
disableDrivers _ req = withFlowHandlerAPI $ do
  -- FIXME: drivers for only one organization?
  Client.callBecknTransportApi (\tok -> client tok req) "disableDrivers"
  where
    client =
      T.client (Proxy @BecknTransport.DisableDriversAPI)

driverLocation :: Id DP.Person -> Common.DriverIds -> FlowHandler Common.DriverLocationRes
driverLocation _ req = withFlowHandlerAPI $ do
  -- FIXME: drivers for only one organization?
  Client.callBecknTransportApi (\tok -> client tok req) "driverLocation"
  where
    client = T.client (Proxy @BecknTransport.DriverLocationAPI)
