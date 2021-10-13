module Fmd where

import Beckn.Utils.Example
import Beckn.Utils.Servant.BaseUrl (showBaseUrlText)
import Data.Time
import EulerHS.Prelude
import FmdWrapper.Common (fmdTestAppBaseUrl)
import Servant.Client
import "fmd-wrapper" Types.Beckn.API.Search
import qualified "fmd-wrapper" Types.Beckn.API.Types as API
import "fmd-wrapper" Types.Beckn.Context
import qualified "fmd-wrapper" Types.Beckn.Domain as Domain

fmdWrapperBaseUrl :: BaseUrl
fmdWrapperBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8018,
      baseUrlPath = "/v1"
    }

buildContext ::
  Action ->
  Text ->
  IO Context
buildContext act tid = do
  now <- getCurrentTime
  return
    Context
      { domain = Domain.FINAL_MILE_DELIVERY,
        country = "IND",
        city = "Bangalore",
        action = act,
        core_version = "0.9.1",
        bap_id = showBaseUrlText fmdTestAppBaseUrl,
        bap_uri = fmdTestAppBaseUrl,
        bpp_id = Nothing,
        bpp_uri = Nothing,
        transaction_id = tid,
        message_id = tid, -- FIXME
        timestamp = now,
        key = Nothing,
        ttl = Nothing
      }

buildFMDSearchReq :: Context -> API.BecknReq SearchIntent
buildFMDSearchReq context =
  API.BecknReq
    { context,
      message = SearchIntent example
    }
