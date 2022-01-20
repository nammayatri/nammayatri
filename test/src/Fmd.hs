module Fmd where

import qualified Beckn.Types.Core.ReqTypes as API
import Beckn.Utils.Servant.BaseUrl (showBaseUrlText)
import Data.Time
import EulerHS.Prelude
import FmdWrapper.Common (fmdTestAppBaseUrl)
import Servant.Client
import "fmd-wrapper" Types.Beckn.API.Search
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
        timestamp = now
      }

buildFMDSearchReq :: Context -> Gps -> Gps -> API.BecknReq SearchIntent
buildFMDSearchReq context pickupGps dropGps =
  API.BecknReq
    { context,
      message =
        SearchIntent
          { intent =
              Intent
                { fulfillment =
                    FulFillmentInfo
                      { start =
                          LocationInfo
                            { location =
                                Location
                                  { gps = pickupGps
                                  }
                            },
                        end =
                          LocationInfo
                            { location =
                                Location
                                  { gps = dropGps
                                  }
                            }
                      }
                }
          }
    }
