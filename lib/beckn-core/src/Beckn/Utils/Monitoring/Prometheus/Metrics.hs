{-# LANGUAGE UndecidableInstances #-}

module Beckn.Utils.Monitoring.Prometheus.Metrics where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Utils.Common
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.Auth
import Beckn.Utils.Servant.Server
import Control.Lens ((?=))
import Data.Default (def)
import Data.Kind (Type)
import Data.Map
import Data.Proxy
import qualified Data.Swagger as DS
import Data.Text as DT
import Data.Typeable (typeRep)
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude as E
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as T
import GHC.Exts (fromList)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Network.HTTP.Types as H
import Network.Wai (Middleware, Request (..))
import Network.Wai.Handler.Warp as W
import Network.Wai.Middleware.Prometheus
import Prometheus
import Prometheus.Metric.GHC (ghcMetrics)
import Prometheus.Metric.Proc
import Servant
import Servant.Client
import Servant.Server.Internal.Delayed (addAuthCheck)
import Servant.Server.Internal.DelayedIO (DelayedIO, delayedFailFatal, withRequest)
import qualified Servant.Swagger as S
import qualified Servant.Swagger.Internal as S

serve :: IO ()
serve = do
  _ <- register ghcMetrics
  _ <- register procMetrics
  _ <- forkIO $ W.run 9999 metricsApp
  return ()

addServantInfo proxy app request respond =
  let mpath = getSanitizedUrl proxy request
      fullpath = DT.intercalate "/" (pathInfo request)
   in instrumentHandlerValue (\_ -> "/" <> fromMaybe fullpath mpath) app request respond
