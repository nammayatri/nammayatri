{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module API.BPP.DriverOffer.Driver
  ( API,
    handler,
  )
where

import qualified "driver-offer-bpp" API.Dashboard.Driver as DriverOfferBpp
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
  DriverDocumentsInfoAPI
    :<|> DriverListAPI
    :<|> DriverActivityAPI
    :<|> EnableDriversAPI
    :<|> DisableDriversAPI
    :<|> DriverLocationAPI

handler :: FlowServer API
handler =
  driverDocuments
    :<|> listDriver
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

type DriverDocumentsInfoAPI =
  "driver"
    :> "documents"
    :> "info"
    :> ApiAuth 'READ_ACCESS 'DRIVERS
    :> Common.DriverDocumentsInfoAPI

driverDocuments :: Id DP.Person -> FlowHandler Common.DriverDocumentsInfoRes
driverDocuments _ = withFlowHandlerAPI $ do
  -- FIXME: drivers for only one organization?
  Client.callDriverOfferApi client "driverDocuments"
  where
    client =
      T.client (Proxy @DriverOfferBpp.DriverDocumentsInfoAPI)

listDriver :: Id DP.Person -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> FlowHandler Common.DriverListRes
listDriver _ mbLimit mbOffset verified rejected pendingdoc phone = withFlowHandlerAPI $ do
  -- FIXME: drivers for only one organization?
  Client.callDriverOfferApi (\tok -> client tok mbLimit mbOffset verified rejected pendingdoc phone) "listDriver"
  where
    client =
      T.client
        (Proxy @DriverOfferBpp.DriverListAPI)

driverActivity :: Id DP.Person -> FlowHandler Common.DriverActivityRes
driverActivity _ = withFlowHandlerAPI $ do
  -- FIXME: drivers for only one organization?
  Client.callDriverOfferApi client "driverActivity"
  where
    client =
      T.client (Proxy @DriverOfferBpp.DriverActivityAPI)

enableDrivers :: Id DP.Person -> Common.DriverIds -> FlowHandler Common.EnableDriversRes
enableDrivers _ req = withFlowHandlerAPI $ do
  -- FIXME: drivers for only one organization?
  Client.callDriverOfferApi (\tok -> client tok req) "enableDrivers"
  where
    client =
      T.client (Proxy @DriverOfferBpp.EnableDriversAPI)

disableDrivers :: Id DP.Person -> Common.DriverIds -> FlowHandler Common.DisableDriversRes
disableDrivers _ req = withFlowHandlerAPI $ do
  -- FIXME: drivers for only one organization?
  Client.callDriverOfferApi (\tok -> client tok req) "disableDrivers"
  where
    client =
      T.client (Proxy @DriverOfferBpp.DisableDriversAPI)

driverLocation :: Id DP.Person -> Maybe Int -> Maybe Int -> Common.DriverIds -> FlowHandler Common.DriverLocationRes
driverLocation _ mbLimit mbOffset req = withFlowHandlerAPI $ do
  -- FIXME: drivers for only one organization?
  Client.callDriverOfferApi (\tok -> client tok mbLimit mbOffset req) "driverLocation"
  where
    client = T.client (Proxy @DriverOfferBpp.DriverLocationAPI)
