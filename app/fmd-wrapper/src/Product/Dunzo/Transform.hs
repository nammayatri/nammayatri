{-# LANGUAGE OverloadedLabels #-}

module Product.Dunzo.Transform where

import App.Types
import Beckn.Types.Common (generateGUID)
import Beckn.Types.Core.Amount
import Beckn.Types.Core.Catalog
import Beckn.Types.Core.Context
import Beckn.Types.Core.DecimalValue
import Beckn.Types.Core.Descriptor
import qualified Beckn.Types.Core.Error as Err
import Beckn.Types.Core.Item
import Beckn.Types.Core.MonetaryValue
import Beckn.Types.Core.Operator
import Beckn.Types.Core.Payment
import Beckn.Types.Core.PaymentPolicy
import Beckn.Types.Core.Person
import Beckn.Types.Core.Price
import Beckn.Types.Core.Quotation
import Beckn.Types.FMD.API.Callback
import Beckn.Types.FMD.API.Cancel
import Beckn.Types.FMD.API.Init
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.API.Select
import Beckn.Types.FMD.API.Status
import Beckn.Types.FMD.API.Track
import Beckn.Types.FMD.Order
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common (throwJsonError400)
import Beckn.Utils.Extra (getCurrentTimeUTC)
import Control.Lens ((?~))
import qualified Data.Text as T
import EulerHS.Prelude hiding (drop)
import External.Dunzo.Types
import Types.Wrapper
import Utils.Common (getClientConfig)

getDunzoConfig :: Organization -> Flow DunzoConfig
getDunzoConfig org = do
  config <- getClientConfig org
  case config of
    Dunzo dzConfig -> return dzConfig

mkQuoteReqFromSearch :: SearchReq -> Flow QuoteReq
mkQuoteReqFromSearch SearchReq {..} = do
  let intent = message ^. #intent
      pickups = intent ^. #_pickups
      drops = intent ^. #_drops
  case (pickups, drops) of
    ([pickup], [drop]) ->
      case (pickup ^. #_location . #_gps, drop ^. #_location . #_gps) of
        (Just pgps, Just dgps) -> do
          plat <- readCoord (pgps ^. #lat)
          plon <- readCoord (pgps ^. #lon)
          dlat <- readCoord (dgps ^. #lat)
          dlon <- readCoord (dgps ^. #lon)
          return $
            QuoteReq
              { pickup_lat = plat,
                pickup_lng = plon,
                drop_lat = dlat,
                drop_lng = dlon,
                category_id = "pickup_drop"
              }
        (Just _, Nothing) -> dropLocationNotFound
        _ -> pickupLocationNotFound
    ([_], _) -> oneDropLocationExpected
    _ -> onePickupLocationExpected
  where
    onePickupLocationExpected = throwJsonError400 "ERR" "ONE_PICKUP_LOCATION_EXPECTED"
    oneDropLocationExpected = throwJsonError400 "ERR" "ONE_DROP_LOCATION_EXPECTED"
    pickupLocationNotFound = throwJsonError400 "ERR" "PICKUP_LOCATION_NOT_FOUND"
    dropLocationNotFound = throwJsonError400 "ERR" "DROP_LOCATION_NOT_FOUND"

mkQuoteReqFromSelect :: SelectReq -> Flow QuoteReq
mkQuoteReqFromSelect SelectReq {..} = do
  let tasks = message ^. (#order . #_tasks)
      task = head tasks
      pickup = task ^. (#_pickup . #_location)
      drop = task ^. (#_drop . #_location)
      pgps = pickup ^. #_gps
      dgps = drop ^. #_gps
  plat <- readCoord (fromJust pgps ^. #lat)
  plon <- readCoord (fromJust pgps ^. #lon)
  dlat <- readCoord (fromJust dgps ^. #lat)
  dlon <- readCoord (fromJust dgps ^. #lon)
  return $
    QuoteReq
      { pickup_lat = plat,
        pickup_lng = plon,
        drop_lat = dlat,
        drop_lng = dlon,
        category_id = "pickup_drop"
      }

readCoord :: Text -> Flow Double
readCoord text = do
  let mCoord = readMaybe $ T.unpack text
  maybe (throwJsonError400 "ERR" "LOCATION_READ_ERROR") pure mCoord

mkOnSearchErrReq :: Organization -> Context -> Error -> Flow OnSearchReq
mkOnSearchErrReq _ context Error {..} = do
  return $
    CallbackReq
      { context = context,
        contents = Left err
      }
  where
    err =
      Err.Error
        { _type = "DOMAIN-ERROR",
          _code = code,
          _path = Nothing,
          _message = Just message
        }

mkOnSearchReq :: Organization -> Context -> QuoteRes -> Flow OnSearchReq
mkOnSearchReq _ context res@QuoteRes {..} = do
  now <- getCurrentTimeUTC
  cid <- generateGUID
  itemid <- generateGUID
  return $
    CallbackReq
      { context = context,
        contents = Right $ OnSearchServices (catalog cid itemid now)
      }
  where
    catalog cid itemid now =
      Catalog
        { _id = cid,
          _categories = [],
          _brands = [],
          _models = [],
          _ttl = now,
          _items = [mkSearchItem itemid res],
          _offers = []
        }

updateContext :: Context -> Text -> Text -> Context
updateContext Context {..} bpId bpNwAddress = Context {_bpp_nw_address = Just bpNwAddress, _bpp_id = Just bpId, ..}

mkSearchItem :: Text -> QuoteRes -> Item
mkSearchItem itemId QuoteRes {..} =
  Item
    { _id = itemId,
      _parent_item_id = Nothing,
      _descriptor = descriptor,
      _price = price,
      _model_id = Nothing,
      _category_id = Just category_id,
      _brand_id = Nothing,
      _promotional = False,
      _ttl = Just $ eta ^. #pickup + eta ^. #dropoff, -- FIX this
      _tags = []
    }
  where
    price =
      Price
        { _currency = "INR",
          _value = Nothing,
          _estimated_value = Just value,
          _computed_value = Nothing,
          _listed_value = Nothing,
          _offered_value = Nothing,
          _minimum_value = Nothing,
          _maximum_value = Nothing
        }
    value = convertAmountToDecimalValue (Amount $ toRational estimated_price)
    descriptor = Descriptor n n n n n [] n n
    n = Nothing

mkQuote :: QuoteRes -> Flow Quotation
mkQuote QuoteRes {..} = do
  qid <- generateGUID
  return $ Quotation {_id = qid, _price = price, _ttl = Nothing}
  where
    price = mkPrice estimated_price
    mkPrice estimatedPrice =
      Price
        { _currency = "INR",
          _value = Nothing,
          _estimated_value = Just $ convertAmountToDecimalValue $ Amount $ toRational estimatedPrice,
          _computed_value = Nothing,
          _listed_value = Nothing,
          _offered_value = Nothing,
          _minimum_value = Nothing,
          _maximum_value = Nothing
        }

mkOnSelectMessage :: Quotation -> SelectReq -> OnSelectMessage
mkOnSelectMessage quote req = OnSelectMessage order quote
  where
    order = req ^. #message . #order

mkOnSelectReq :: SelectReq -> OnSelectMessage -> OnSelectReq
mkOnSelectReq req msg =
  CallbackReq
    { context = req ^. #context,
      contents = Right msg
    }

mkOnSelectErrReq :: SelectReq -> Error -> OnSelectReq
mkOnSelectErrReq req Error {..} =
  CallbackReq
    { context = req ^. #context,
      contents = Left mkError
    }
  where
    mkError =
      Err.Error
        { _type = "DOMAIN-ERROR",
          _code = code,
          _path = Nothing,
          _message = Just message
        }

mkOnInitMessage :: Text -> Order -> PaymentPolicy -> InitReq -> QuoteRes -> InitResMessage
mkOnInitMessage orderId order terms req QuoteRes {..} =
  InitResMessage $
    order & #_id ?~ orderId & #_payment ?~ mkPayment & #_billing .~ billing
  where
    mkPayment =
      Payment
        { _transaction_id = Nothing,
          _type = Just "PRE-FULFILLMENT",
          _payer = Nothing,
          _payee = Nothing,
          _method = "RTGS",
          _amount = price,
          _state = Nothing,
          _due_date = Nothing,
          _duration = Nothing,
          _terms = Just terms
        }
    price =
      MonetaryValue
        { _currency = "INR",
          _value = convertAmountToDecimalValue $ Amount $ toRational estimated_price
        }
    billing = req ^. #message . #order . #_billing

mkOnInitReq :: InitReq -> InitResMessage -> OnInitReq
mkOnInitReq req msg =
  CallbackReq
    { context = req ^. #context,
      contents = Right msg
    }

mkOnInitErrReq :: InitReq -> Error -> Flow OnInitReq
mkOnInitErrReq req Error {..} = do
  return $
    CallbackReq
      { context = req ^. #context,
        contents = Left mkError
      }
  where
    mkError =
      Err.Error
        { _type = "DOMAIN-ERROR",
          _code = code,
          _path = Nothing,
          _message = Just message
        }

{-# ANN mkOnStatusMessage ("HLint: ignore Use <$>" :: String) #-}
mkOnStatusMessage :: Text -> Order -> TaskStatus -> Flow StatusResMessage
mkOnStatusMessage orgName order status = do
  now <- getCurrentTimeUTC
  return $ StatusResMessage (updateOrder now)
  where
    updateOrder cTime =
      order & #_state ?~ show (status ^. #state)
        & #_updated_at .~ cTime
        & #_tasks .~ (updateTask cTime <$> (order ^. #_tasks))

    updateTask cTime task =
      task & #_agent .~ (getAgent <$> status ^. #runner)
        & #_state .~ show (status ^. #state)
        & #_updated_at ?~ cTime

    getAgent runner =
      Operator
        { _name = Name n n (runner ^. #name) n n n,
          _image = n,
          _dob = n,
          _organization_name = Just orgName,
          _gender = n,
          _email = n,
          _phones = [runner ^. #phone_number],
          _experience = n
        }

    n = Nothing

mkOnStatusReq :: StatusReq -> StatusResMessage -> Flow OnStatusReq
mkOnStatusReq req msg =
  return $
    CallbackReq
      { context = req ^. #context,
        contents = Right msg
      }

mkOnStatusErrReq :: StatusReq -> Error -> Flow OnStatusReq
mkOnStatusErrReq req Error {..} =
  return $
    CallbackReq
      { context = req ^. #context,
        contents = Left mkError
      }
  where
    mkError =
      Err.Error
        { _type = "DOMAIN-ERROR",
          _code = code,
          _path = Nothing,
          _message = Just message
        }

mkOnTrackErrReq :: TrackReq -> Flow OnTrackReq
mkOnTrackErrReq req = do
  return $
    CallbackReq
      { context = req ^. #context,
        contents = Left mkError
      }
  where
    mkError =
      Err.Error
        { _type = "DOMAIN-ERROR",
          _code = "FMD000",
          _path = Nothing,
          _message = Just "NO_TRACKING_URL"
        }

mkOnCancelReq :: CancelReq -> Order -> Flow OnCancelReq
mkOnCancelReq req order = do
  return $
    CallbackReq
      { context = req ^. #context,
        contents = Right cancel
      }
  where
    cancel =
      CancelResMessage
        { policies = [],
          reasons = [],
          price = Nothing,
          order = order
        }

mkOnCancelErrReq :: CancelReq -> Error -> Flow OnCancelReq
mkOnCancelErrReq req Error {..} = do
  return $
    CallbackReq
      { context = req ^. #context,
        contents = Left mkError
      }
  where
    mkError =
      Err.Error
        { _type = "DOMAIN-ERROR",
          _code = code,
          _path = Nothing,
          _message = Just message
        }
