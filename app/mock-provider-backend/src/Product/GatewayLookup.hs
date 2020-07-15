module Product.GatewayLookup
  ( lookup,
    lookupBaseUrl,
  )
where

import Beckn.Types.Common
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import Servant.Client (BaseUrl (..), Scheme (..))
import qualified System.Environment as SE

lookup :: FlowR r (String, Int)
lookup = do
  EL.runIO $
    (,)
      <$> (fromMaybe "localhost" <$> SE.lookupEnv "GATEWAY_HOST")
      <*> (fromMaybe 8015 . (>>= readMaybe) <$> SE.lookupEnv "GATEWAY_PORT")

lookupBaseUrl :: FlowR r BaseUrl
lookupBaseUrl = do
  (host, port) <- lookup
  return
    BaseUrl
      { baseUrlScheme = Http,
        baseUrlHost = host,
        baseUrlPort = port,
        baseUrlPath = ""
      }
