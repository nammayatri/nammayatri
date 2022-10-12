{-# LANGUAGE AllowAmbiguousTypes #-}

module BAPClient.Yatri
  ( callYatriBAP,
    CustomerAPIs (..),
    YatriAPIs (..),
  )
where

import "app-backend" API.Dashboard as Dashboard
import Beckn.Prelude
import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Utils.Common hiding (callAPI)
import Domain.Types.ServerName
import qualified EulerHS.Types as Euler
import Tools.Client

newtype YatriAPIs = YatriAPIs
  { customers :: CustomerAPIs
  }

newtype CustomerAPIs = CustomerAPIs
  { customerList :: Maybe Integer -> Maybe Integer -> Euler.EulerClient Text
  }

mkYatriAPIs :: Text -> YatriAPIs
mkYatriAPIs token = do
  let customers = CustomerAPIs {..}
  YatriAPIs {..}
  where
    customerList = Euler.client (Proxy :: Proxy Dashboard.API') token

callYatriBAP ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI YatriAPIs m r b c
  ) =>
  (YatriAPIs -> b) ->
  c
callYatriBAP = callServerAPI @_ @m @r APP_BACKEND_YATRI mkYatriAPIs "callYatriBAP"
