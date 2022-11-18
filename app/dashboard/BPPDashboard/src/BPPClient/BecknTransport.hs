{-# LANGUAGE AllowAmbiguousTypes #-}

module BPPClient.BecknTransport
  ( callBecknTransportBPP,
    BecknTransportAPIs (..),
    DriversAPIs (..),
  )
where

import "beckn-transport" API.Dashboard as Dashboard
import Beckn.Prelude
import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Beckn.Utils.Common hiding (callAPI)
import qualified Dashboard.Common.Driver as Common
import Domain.Types.ServerName
import qualified EulerHS.Types as Euler
import Servant
import Tools.Client

newtype BecknTransportAPIs = BecknTransportAPIs
  { drivers :: DriversAPIs
  }

data DriversAPIs = DriversAPIs
  { listDrivers :: Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Euler.EulerClient Common.DriverListRes,
    driverActivity :: Euler.EulerClient Common.DriverActivityRes,
    enableDrivers :: Common.DriverIds -> Euler.EulerClient Common.EnableDriversRes,
    disableDrivers :: Common.DriverIds -> Euler.EulerClient Common.DisableDriversRes,
    driverLocation :: Maybe Int -> Maybe Int -> Common.DriverIds -> Euler.EulerClient Common.DriverLocationRes,
    driverInfo :: Maybe Text -> Maybe Text -> Euler.EulerClient Common.DriverInfoRes,
    deleteDriver :: Id Common.Driver -> Euler.EulerClient APISuccess
  }

mkBecknTransportAPIs :: Text -> BecknTransportAPIs
mkBecknTransportAPIs token = do
  let drivers = DriversAPIs {..}
  BecknTransportAPIs {..}
  where
    listDrivers
      :<|> driverActivity
      :<|> enableDrivers
      :<|> disableDrivers
      :<|> driverLocation
      :<|> driverInfo
      :<|> deleteDriver = Euler.client (Proxy :: Proxy Dashboard.API) token

callBecknTransportBPP ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI BecknTransportAPIs m r b c
  ) =>
  (BecknTransportAPIs -> b) ->
  c
callBecknTransportBPP = callServerAPI @_ @m @r BECKN_TRANSPORT mkBecknTransportAPIs "callBecknTransportBPP"
