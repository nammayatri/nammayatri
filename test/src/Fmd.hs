module Fmd where

import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import Beckn.Types.Core.Quotation
import Beckn.Types.FMD.API.Confirm
import Beckn.Types.FMD.API.Init
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.API.Select
import Beckn.Types.FMD.API.Update
import Beckn.Types.FMD.Order
import Beckn.Utils.Common
import Data.Time
import EulerHS.Prelude
import Servant.Client

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
      { _domain = FINAL_MILE_DELIVERY,
        _action = act,
        _country = Just "IND",
        _city = Nothing,
        _core_version = Just "0.8.0",
        _domain_version = Just "0.8.3",
        _bap_uri = bapBaseUrl,
        _bpp_uri = bppBaseUrl,
        _transaction_id = tid,
        _message_id = tid,
        _timestamp = now
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
      message = InitOrder $ order {_quotation = Just (Quotation quoteId Nothing Nothing Nothing)}
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
