module Fmd where

import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import qualified Beckn.Types.Core.Migration.Context as Mig
import qualified Beckn.Types.Core.Migration.Domain as Mig
import Beckn.Types.Core.Quotation
import Beckn.Utils.Example
import Data.Time
import EulerHS.Prelude
import FmdWrapper.Common (fmdTestAppBaseUrl)
import Servant.Client
import "fmd-wrapper" Types.Beckn.API.Init
import "fmd-wrapper" Types.Beckn.API.Search
import "fmd-wrapper" Types.Beckn.API.Select
import qualified "fmd-wrapper" Types.Beckn.API.Types as API
import "fmd-wrapper" Types.Beckn.API.Update
import "fmd-wrapper" Types.Beckn.FmdOrder

fmdWrapperBaseUrl :: BaseUrl
fmdWrapperBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8018,
      baseUrlPath = "/v1"
    }

buildContext ::
  Text ->
  Text ->
  Maybe BaseUrl ->
  Maybe BaseUrl ->
  IO Context
buildContext act tid bapBaseUrl bppBaseUrl = do
  now <- getCurrentTime
  return
    Context
      { domain = FINAL_MILE_DELIVERY,
        action = act,
        country = Just "IND",
        city = Nothing,
        core_version = Just "0.8.0",
        domain_version = Just "0.8.3",
        bap_uri = bapBaseUrl,
        bpp_uri = bppBaseUrl,
        transaction_id = tid,
        message_id = tid, -- FIXME
        timestamp = now,
        ttl = Nothing
      }

buildContextMig ::
  Text ->
  Text ->
  Maybe BaseUrl ->
  IO Mig.Context
buildContextMig act tid bppBaseUrl = do
  now <- getCurrentTime
  return
    Mig.Context
      { domain = Mig.Domain "FINAL-MILE-DELIVERY",
        country = "IND",
        city = "Bangalore",
        action = act,
        core_version = "0.9.1",
        bap_id = fmdTestAppBaseUrl,
        bap_uri = fmdTestAppBaseUrl,
        bpp_id = Nothing,
        bpp_uri = bppBaseUrl,
        transaction_id = tid,
        message_id = tid, -- FIXME
        timestamp = now,
        key = Nothing,
        ttl = Nothing
      }

buildFMDSearchReq :: Mig.Context -> API.BecknReq SearchIntent
buildFMDSearchReq context =
  API.BecknReq
    { context,
      message = SearchIntent example
    }

buildFMDSelectReq :: Context -> SelectReq
buildFMDSelectReq context =
  SelectReq
    { context,
      message = SelectOrder example
    }

buildFMDInitReq :: Context -> Text -> InitReq
buildFMDInitReq context quoteId = do
  let order = example
  InitReq
    { context,
      message = InitOrder $ order {quotation = Just (Quotation quoteId Nothing Nothing Nothing)}
    }

buildFMDConfirmReq :: Mig.Context -> API.BecknReq API.OrderObject
buildFMDConfirmReq context =
  API.BecknReq
    { context,
      message = API.OrderObject example
    }

buildFMDUpdateReq :: Context -> UpdateReq
buildFMDUpdateReq context =
  UpdateReq
    { context,
      message = UpdateReqMessage "update_pickup_location" example
    }
