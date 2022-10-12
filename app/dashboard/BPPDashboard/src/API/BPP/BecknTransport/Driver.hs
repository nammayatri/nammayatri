{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module API.BPP.BecknTransport.Driver
  ( API,
    handler,
  )
where

import qualified BPPClient.BecknTransport as Client
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver as Common
import "lib-dashboard" Domain.Types.Person as DP
import "lib-dashboard" Environment
import Servant
import "lib-dashboard" Tools.Auth

type API =
  "driver"
    :> ( DriverListAPI
           :<|> DriverActivityAPI
           :<|> EnableDriversAPI
           :<|> DisableDriversAPI
           :<|> DriverLocationAPI
       )

type DriverListAPI =
  "list"
    :> ApiAuth 'READ_ACCESS 'DRIVERS
    :> Common.DriverListAPI

type DriverActivityAPI =
  "activity"
    :> ApiAuth 'READ_ACCESS 'DRIVERS
    :> Common.DriverActivityAPI

type EnableDriversAPI =
  "enable"
    :> ApiAuth 'WRITE_ACCESS 'DRIVERS
    :> Common.EnableDriversAPI

type DisableDriversAPI =
  "disable"
    :> ApiAuth 'WRITE_ACCESS 'DRIVERS
    :> Common.DisableDriversAPI

type DriverLocationAPI =
  "location"
    :> ApiAuth 'READ_ACCESS 'DRIVERS
    :> Common.DriverLocationAPI

handler :: FlowServer API
handler =
  listDriver
    :<|> driverActivity
    :<|> enableDrivers
    :<|> disableDrivers
    :<|> driverLocation

listDriver :: Id DP.Person -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> FlowHandler Common.DriverListRes
listDriver _ mbLimit mbOffset verified rejected pendingdoc phone = withFlowHandlerAPI $ do
  Client.callBecknTransportBPP (.drivers.listDrivers) mbLimit mbOffset verified rejected pendingdoc phone

driverActivity :: Id DP.Person -> FlowHandler Common.DriverActivityRes
driverActivity _ = withFlowHandlerAPI $ do
  Client.callBecknTransportBPP (.drivers.driverActivity)

enableDrivers :: Id DP.Person -> Common.DriverIds -> FlowHandler Common.EnableDriversRes
enableDrivers _ req = withFlowHandlerAPI $ do
  Client.callBecknTransportBPP (.drivers.enableDrivers) req

disableDrivers :: Id DP.Person -> Common.DriverIds -> FlowHandler Common.DisableDriversRes
disableDrivers _ req = withFlowHandlerAPI $ do
  Client.callBecknTransportBPP (.drivers.disableDrivers) req

driverLocation :: Id DP.Person -> Maybe Int -> Maybe Int -> Common.DriverIds -> FlowHandler Common.DriverLocationRes
driverLocation _ mbLimit mbOffset req = withFlowHandlerAPI $ do
  Client.callBecknTransportBPP (.drivers.driverLocation) mbLimit mbOffset req
