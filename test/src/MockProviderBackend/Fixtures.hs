module MockProviderBackend.Fixtures where

import qualified "mock-provider-backend" App as MockProviderBE
import "mock-provider-backend" App.Handlers as MockProviderRoutes
import Beckn.Types.Common as Common
import Beckn.Types.Core.Context
import Beckn.Utils.Common
import Data.Time
import EulerHS.Prelude
import Servant.Client

buildContext :: Text -> Text -> UTCTime -> Context
buildContext act tid utcTime =
  Context
    { _domain = "FINAL-MILE-DELIVERY",
      _action = act,
      _country = Nothing,
      _city = Nothing,
      _core_version = Just "0.8.0",
      _domain_version = Just "0.7.0",
      _bap_id = Nothing,
      _bg_id = Nothing,
      _bpp_id = Nothing,
      _bap_nw_address = Nothing,
      _bg_nw_address = Nothing,
      _bpp_nw_address = Nothing,
      _request_transaction_id = tid,
      _timestamp = utcTime,
      _token = Nothing
    }

mockProviderBaseUrl :: BaseUrl
mockProviderBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8017,
      baseUrlPath = "/v1"
    }

startServer :: IO ThreadId
startServer = forkIO MockProviderBE.runMockProvider

