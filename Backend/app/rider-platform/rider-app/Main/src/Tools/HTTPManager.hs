module Tools.HTTPManager where

import qualified Data.HashMap.Internal as HMap
import qualified Data.Text as DT
import EulerHS.Prelude
import Network.HTTP.Client as Http
import Network.HTTP.Client.TLS as Http

prepareCRISHttpManager :: Int -> HMap.HashMap DT.Text Http.ManagerSettings
prepareCRISHttpManager timeout =
  HMap.singleton (DT.pack crisHttpManagerKey) $
    Http.tlsManagerSettings {Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)}

crisHttpManagerKey :: String
crisHttpManagerKey = "cris-http-manager"
