{-# LANGUAGE OverloadedLabels #-}

module App.Utils where

import App.Types
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
import Data.Time
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import Servant.Client (BaseUrl, parseBaseUrl)
import System.Environment (lookupEnv)

address :: Address
address =
  Address
    { door = "#817",
      building = "Juspay Apartments",
      street = "27th Main",
      area = "8th Block Koramangala",
      city = "Bangalore",
      country = "India",
      area_code = "560047"
    }

gps :: GPS
gps =
  GPS
    { lat = "12.9401108",
      lon = "77.6206631"
    }

location :: Location
location =
  Location
    { _type = "gps",
      _gps = Just gps,
      _address = Just address,
      _station_code = Nothing,
      _area_code = Nothing,
      _city = Nothing,
      _country = Nothing,
      _circle = Nothing,
      _polygon = Nothing,
      _3dspace = Nothing
    }

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
            _drops = [PickupDrop location utcTime],
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
                _pickup = PickupOrDrop location [] example,
                _drop = PickupOrDrop location [] example,
                _package = example, -- FIXME: references item and price
                _agent = example, -- FIXME: we can't fill this
                _vehicle = example, -- FIXME: we can't fill this
                _created_at = Just now,
                _updated_at = Just now
              }
          ],
        _billing = Nothing,
        _payment = Nothing
      }

buildContext :: Text -> Text -> Flow Context
buildContext act tid = do
  utcTime <- getCurrTime
  bapNwAddr <- EL.runIO $ lookupEnv "MOCK_APP_NW_ADDRESS"
  return $
    Context
      { _domain = FINAL_MILE_DELIVERY,
        _action = act,
        _country = Nothing,
        _city = Nothing,
        _core_version = Just "0.8.0",
        _domain_version = Just "0.7.0",
        _ac_id = fromString <$> bapNwAddr,
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
buildInitReq ctx quotId =
  return $
    InitReq
      { context = ctx {_action = "init"},
        message = InitReqMessage quotId example
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
  parseBaseUrl . toString =<< context ^. #_ac_id

updateCaller :: Context -> Flow Context
updateCaller ctx = do
  bapNwAddr <- EL.runIO $ lookupEnv "MOCK_APP_NW_ADDRESS"
  return $ ctx {_ac_id = fromString <$> bapNwAddr}
