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
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import Domain.Types.ServerName
import qualified EulerHS.Types as Euler
import Servant
import Tools.Auth.Merchant (CheckedShortId)
import Tools.Client

newtype BecknTransportAPIs = BecknTransportAPIs
  { drivers :: DriversAPIs
  }

data DriversAPIs = DriversAPIs
  { listDrivers :: Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Text -> Euler.EulerClient Common.DriverListRes,
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

mkBecknTransportAPIs :: CheckedShortId DM.Merchant -> Text -> BecknTransportAPIs
mkBecknTransportAPIs merchantId token = do
  let drivers = DriversAPIs {..}
  BecknTransportAPIs {..}
  where
    listDrivers
      :<|> driverActivity
      :<|> enableDriver
      :<|> disableDriver
      :<|> driverLocation
      :<|> driverInfo
      :<|> deleteDriver
      :<|> unlinkVehicle
      :<|> updatePhoneNumber
      :<|> addVehicle = clientWithMerchant (Proxy :: Proxy BPP.API') merchantId token

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
