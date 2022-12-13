{-# LANGUAGE AllowAmbiguousTypes #-}

module BPPClient.BecknTransport
  ( callBecknTransportBPP,
    BecknTransportAPIs (..),
    DriversAPIs (..),
  )
where

import "beckn-transport" API.Dashboard as BPP
import Beckn.Prelude
import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Beckn.Utils.Common hiding (callAPI)
import qualified Dashboard.Common.Driver as Common
import qualified Dashboard.Common.Ride as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import Domain.Types.ServerName
import qualified EulerHS.Types as Euler
import Servant
import Tools.Auth.Merchant (CheckedShortId)
import Tools.Client

data BecknTransportAPIs = BecknTransportAPIs
  { drivers :: DriversAPIs,
    rides :: RidesAPIs
  }

data DriversAPIs = DriversAPIs
  { listDrivers :: Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Euler.EulerClient Common.DriverListRes,
    driverActivity :: Euler.EulerClient Common.DriverActivityRes,
    enableDriver :: Id Common.Driver -> Euler.EulerClient APISuccess,
    disableDriver :: Id Common.Driver -> Euler.EulerClient APISuccess,
    driverLocation :: Maybe Int -> Maybe Int -> Common.DriverIds -> Euler.EulerClient Common.DriverLocationRes,
    driverInfo :: Maybe Text -> Maybe Text -> Euler.EulerClient Common.DriverInfoRes,
    deleteDriver :: Id Common.Driver -> Euler.EulerClient APISuccess,
    unlinkVehicle :: Id Common.Driver -> Euler.EulerClient APISuccess,
    updatePhoneNumber :: Id Common.Driver -> Common.UpdatePhoneNumberReq -> Euler.EulerClient APISuccess,
    addVehicle :: Id Common.Driver -> Common.AddVehicleReq -> Euler.EulerClient APISuccess
  }

data RidesAPIs = RidesAPIs
  { rideList :: Maybe Int -> Maybe Int -> Maybe Common.BookingStatus -> Maybe (Id Common.Ride) -> Maybe Text -> Maybe Text -> Euler.EulerClient Common.RideListRes,
    rideStart :: Id Common.Ride -> Common.StartRideReq -> Euler.EulerClient APISuccess,
    rideEnd :: Id Common.Ride -> Common.EndRideReq -> Euler.EulerClient APISuccess,
    rideCancel :: Id Common.Ride -> Common.CancelRideReq -> Euler.EulerClient APISuccess,
    rideInfo :: Id Common.Ride -> Euler.EulerClient Common.RideInfoRes
  }

mkBecknTransportAPIs :: CheckedShortId DM.Merchant -> Text -> BecknTransportAPIs
mkBecknTransportAPIs merchantId token = do
  let drivers = DriversAPIs {..}
  let rides = RidesAPIs {..}
  BecknTransportAPIs {..}
  where
    driversClient
      :<|> ridesClient = clientWithMerchant (Proxy :: Proxy BPP.API') merchantId token

    listDrivers
      :<|> driverActivity
      :<|> enableDriver
      :<|> disableDriver
      :<|> driverLocation
      :<|> driverInfo
      :<|> deleteDriver
      :<|> unlinkVehicle
      :<|> updatePhoneNumber
      :<|> addVehicle = driversClient

    rideList
      :<|> rideStart
      :<|> rideEnd
      :<|> rideCancel
      :<|> rideInfo = ridesClient

callBecknTransportBPP ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI BecknTransportAPIs m r b c
  ) =>
  CheckedShortId DM.Merchant ->
  (BecknTransportAPIs -> b) ->
  c
callBecknTransportBPP merchantId = callServerAPI @_ @m @r BECKN_TRANSPORT (mkBecknTransportAPIs merchantId) "callBecknTransportBPP"
