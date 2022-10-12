{-# LANGUAGE AllowAmbiguousTypes #-}

module BAPClient.ARDU
  ( callARDUBAP,
    CustomerAPIs (..),
    ARDUAPIs (..),
  )
where

import "app-backend" API.Dashboard as Dashboard
import Beckn.Prelude
import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Utils.Common hiding (callAPI)
import Domain.Types.ServerName
import qualified EulerHS.Types as Euler
import Tools.Client

newtype ARDUAPIs = ARDUAPIs
  { customers :: CustomerAPIs
  }

newtype CustomerAPIs = CustomerAPIs
  { customerList :: Maybe Integer -> Maybe Integer -> Euler.EulerClient Text
  }

mkARDUAPIs :: Text -> ARDUAPIs
mkARDUAPIs token = do
  let customers = CustomerAPIs {..}
  ARDUAPIs {..}
  where
    customerList = Euler.client (Proxy :: Proxy Dashboard.API') token

callARDUBAP ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI ARDUAPIs m r b c
  ) =>
  (ARDUAPIs -> b) ->
  c
callARDUBAP = callServerAPI @_ @m @r APP_BACKEND_ARDU mkARDUAPIs "callARDUBAP"
