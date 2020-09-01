{-# LANGUAGE OverloadedLabels #-}

module App.Utils where

import App.Types
import Beckn.Types.Core.Address
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import Beckn.Types.Core.Location
import Beckn.Types.FMD.API.Confirm
import Beckn.Types.FMD.API.Init
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.API.Select
import Beckn.Types.FMD.Intent
import Beckn.Types.FMD.Item
import Beckn.Types.FMD.Order
import Beckn.Types.FMD.Task
import Beckn.Utils.Common
import Control.Lens.Prism (_Just)
import Data.Time
import EulerHS.Prelude
import Servant.Client (BaseUrl, parseBaseUrl)

address :: Address
address =
  Address
    { _name = "Address",
      _door = Just "#817",
      _building = Just "Juspay Apartments",
      _street = Just "27th Main",
      _city = "Bangalore",
      _state = "Karnataka",
      _country = "India",
      _area_code = "560047",
      _locality = Just "8th Block Koramangala",
      _ward = Nothing
    }

gps :: GPS
gps =
  GPS
    { lat = "12.9401108",
      lon = "77.6206631"
    }

gps2 :: GPS
gps2 =
  GPS
    { lat = "12.9401108",
      lon = "77.6306631"
    }

location :: Location
location = gpsLocation gps

-- In some cases (e.g. Dunzo node) it is important to have different
-- pickup and drop locations
location2 :: Location
location2 = gpsLocation gps2

buildIntent :: UTCTime -> SearchIntent
buildIntent utcTime =
  SearchIntent
    { intent =
        Intent
          { _query_string = Nothing,
            _provider_id = Nothing,
            _category_id = Nothing,
            _item_id = Nothing,
            _pickups = [PickupDrop location utcTime],
            _drops = [PickupDrop location2 utcTime],
            _packages = [],
            _tags = Nothing
          }
    }

buildDraftOrder :: Text -> Flow Order
buildDraftOrder itemId = do
  now <- getCurrTime
  return $
    Order
      { _id = Just "draft-task-1",
        _state = Nothing,
        _items = [example {_id = Just itemId}],
        _created_at = now,
        _updated_at = now,
        _tasks =
          [ Task
              { _id = "draft-task-1",
                _item_id = itemId,
                _next_task_id = Nothing,
                _previous_task_id = Nothing,
                _state = "", -- FIXME: no relevant value in spec
                _pickup = PickupOrDrop location [] example Nothing,
                _drop = PickupOrDrop location2 [] example Nothing,
                _return = PickupOrDrop location [] example Nothing,
                _package = example, -- FIXME: references item and price
                _agent = example, -- FIXME: we can't fill this
                _vehicle = example, -- FIXME: we can't fill this
                _created_at = Just now,
                _updated_at = Just now
              }
          ],
        _billing = Nothing,
        _payment = Nothing,
        _update_action = Nothing,
        _quotation = Nothing,
        _type = Nothing,
        _prev_order_id = Nothing,
        _return_reason_id = Nothing,
        _cancellation_reasons = [],
        _return_reasons = []
      }

buildContext :: Text -> Text -> Flow Context
buildContext act tid = do
  utcTime <- getCurrTime
  bapNwAddr <- nwAddress <$> ask
  return $
    Context
      { _domain = FINAL_MILE_DELIVERY,
        _action = act,
        _country = Nothing,
        _city = Nothing,
        _core_version = Just "0.8.0",
        _domain_version = Just "0.8.2",
        _bap_uri = bapNwAddr,
        _bpp_uri = Nothing,
        _transaction_id = tid,
        _message_id = tid,
        _timestamp = utcTime
      }

getFutureTime :: Flow UTCTime
getFutureTime =
  -- Generate a time 2 hours in to the future else booking will fail
  addUTCTime 7200 <$> getCurrTime

buildSearchReq :: Text -> Flow SearchReq
buildSearchReq tid = do
  ctx <- buildContext "search" tid
  now <- getCurrTime
  let intent = buildIntent now
  pure $ SearchReq ctx intent

buildSelectReq :: Context -> Text -> Flow SelectReq
buildSelectReq ctx itemId = do
  order <- buildDraftOrder itemId
  return $
    SelectReq
      { context = ctx {_action = "select"},
        message = DraftOrder order
      }

buildInitReq :: Context -> Text -> Flow InitReq
buildInitReq ctx quotId = do
  let order = example
  return $
    InitReq
      { context = ctx {_action = "init"},
        message = InitOrder $ order & #_quotation . _Just . #_id .~ quotId
      }

buildConfirmReq :: Context -> Flow ConfirmReq
buildConfirmReq ctx =
  return $
    ConfirmReq
      { context = ctx {_action = "confirm"},
        message = ConfirmReqMessage example
      }

bppUrl :: Context -> Maybe BaseUrl
bppUrl context =
  parseBaseUrl . toString =<< context ^. #_bpp_uri

updateCaller :: Context -> Flow Context
updateCaller ctx = do
  bppNwAddr <- nwAddress <$> ask
  return $ ctx {_bap_uri = bppNwAddr}
