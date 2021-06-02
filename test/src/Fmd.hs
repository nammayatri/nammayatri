module Fmd where

import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import Beckn.Types.Core.Quotation
import Beckn.Utils.Example
import Data.Time
import EulerHS.Prelude
import Servant.Client
import "fmd-wrapper" Types.Beckn.API.Confirm
import "fmd-wrapper" Types.Beckn.API.Init
import "fmd-wrapper" Types.Beckn.API.Search
import "fmd-wrapper" Types.Beckn.API.Select
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
        message_id = tid,
        timestamp = now,
        ttl = Nothing
      }

buildFMDSearchReq :: Context -> SearchReq
buildFMDSearchReq context =
  SearchReq
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

buildFMDConfirmReq :: Context -> ConfirmReq
buildFMDConfirmReq context =
  ConfirmReq
    { context,
      message = ConfirmReqMessage example
    }

buildFMDUpdateReq :: Context -> UpdateReq
buildFMDUpdateReq context =
  UpdateReq
    { context,
      message = UpdateReqMessage "update_pickup_location" example
    }
