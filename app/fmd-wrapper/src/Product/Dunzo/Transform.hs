{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Dunzo.Transform where

import App.Types
import Beckn.Types.API.Callback
import Beckn.Types.Common (generateGUID)
import qualified Beckn.Types.Core.Address as CoreAddr
import Beckn.Types.Core.Amount
import Beckn.Types.Core.Context
import Beckn.Types.Core.DecimalValue
import Beckn.Types.Core.Descriptor
import qualified Beckn.Types.Core.Error as Err
import Beckn.Types.Core.Item
import qualified Beckn.Types.Core.Location as CoreLoc
import Beckn.Types.Core.MonetaryValue
import Beckn.Types.Core.Operator
import Beckn.Types.Core.Payment
import Beckn.Types.Core.Person
import Beckn.Types.Core.Price
import Beckn.Types.Core.Quotation
import Beckn.Types.FMD.API.Cancel
import Beckn.Types.FMD.API.Confirm
import Beckn.Types.FMD.API.Init
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.API.Select
import Beckn.Types.FMD.API.Status
import Beckn.Types.FMD.API.Track
import Beckn.Types.FMD.API.Update
import Beckn.Types.FMD.Catalog
import Beckn.Types.FMD.Order
import Beckn.Types.FMD.Task
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common (fromMaybeM500, getCurrTime, headMaybe, throwJsonError400)
import Control.Lens ((?~))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified EulerHS.Language as L
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
      { context = context & #_action .~ "on_search",
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
  now <- getCurrTime
  cid <- generateGUID
  itemid <- generateGUID
  return $
    CallbackReq
      { context = context & #_action .~ "on_search",
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
          _offers = [],
          _package_categories = []
        }

updateBppUri :: Context -> Text -> Context
updateBppUri Context {..} bpNwAddress = Context {_bpp_uri = Just bpNwAddress, ..}

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
  return $ Quotation {_id = qid, _price = price, _ttl = Nothing, _breakup = []}
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

mkOnSelectReq :: Context -> OnSelectMessage -> OnSelectReq
mkOnSelectReq context msg =
  CallbackReq
    { context = context & #_action .~ "on_select",
      contents = Right msg
    }

mkOnSelectErrReq :: Context -> Error -> OnSelectReq
mkOnSelectErrReq context Error {..} =
  CallbackReq
    { context = context & #_action .~ "on_select",
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

mkOnInitMessage :: Text -> Order -> DunzoConfig -> InitReq -> QuoteRes -> InitResMessage
mkOnInitMessage orderId order conf req QuoteRes {..} =
  InitResMessage $
    order & #_id ?~ orderId & #_payment ?~ mkPayment & #_billing .~ billing
  where
    mkPayment =
      Payment
        { _transaction_id = Nothing,
          _type = Just "PRE-FULFILLMENT",
          _payer = Nothing,
          _payee = Just $ conf ^. #payee,
          _method = ["RTGS"],
          _amount = price,
          _state = Nothing,
          _due_date = Nothing,
          _duration = Nothing,
          _terms = Just $ conf ^. #paymentPolicy
        }
    price =
      MonetaryValue
        { _currency = "INR",
          _value = convertAmountToDecimalValue $ Amount $ toRational estimated_price
        }
    billing = req ^. #message . #order . #_billing

mkOnInitReq :: Context -> InitResMessage -> OnInitReq
mkOnInitReq context msg =
  CallbackReq
    { context = context & #_action .~ "on_init",
      contents = Right msg
    }

mkOnInitErrReq :: Context -> Error -> Flow OnInitReq
mkOnInitErrReq context Error {..} = do
  return $
    CallbackReq
      { context = context & #_action .~ "on_init",
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
  now <- getCurrTime
  return $ StatusResMessage (updateOrder orgName now order status)

updateOrder :: Text -> UTCTime -> Order -> TaskStatus -> Order
updateOrder orgName cTime order status =
  order & #_state ?~ show (status ^. #state)
    & #_updated_at .~ cTime
    & #_tasks .~ (updateTask <$> (order ^. #_tasks))
  where
    updateTask task =
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

mkOnStatusReq :: Context -> StatusResMessage -> Flow OnStatusReq
mkOnStatusReq context msg =
  return $
    CallbackReq
      { context = context & #_action .~ "on_status",
        contents = Right msg
      }

mkOnStatusErrReq :: Context -> Error -> Flow OnStatusReq
mkOnStatusErrReq context Error {..} =
  return $
    CallbackReq
      { context = context & #_action .~ "on_status",
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

mkOnTrackErrReq :: Context -> Flow OnTrackReq
mkOnTrackErrReq context = do
  return $
    CallbackReq
      { context = context & #_action .~ "on_track",
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

mkOnCancelReq :: Context -> Order -> Flow OnCancelReq
mkOnCancelReq context order = do
  return $
    CallbackReq
      { context = context & #_action .~ "on_cancel",
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

mkOnCancelErrReq :: Context -> Error -> Flow OnCancelReq
mkOnCancelErrReq context Error {..} = do
  return $
    CallbackReq
      { context = context & #_action .~ "on_cancel",
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

mkCreateTaskReq :: Order -> Flow CreateTaskReq
mkCreateTaskReq order = do
  orderId <- order ^. #_id & fromMaybeM500' "ORDER_ID_MISSING"
  let [task] = order ^. #_tasks
  let pickup = task ^. #_pickup
  let drop = task ^. #_drop
  pickupDet <- mkLocationDetails pickup
  dropDet <- mkLocationDetails drop
  senderDet <- mkPersonDetails pickup
  receiverDet <- mkPersonDetails drop
  return $
    CreateTaskReq
      { request_id = orderId,
        pickup_details = pickupDet,
        drop_details = dropDet,
        sender_details = senderDet,
        receiver_details = receiverDet,
        special_instructions = "Handle with care",
        package_approx_value = -1.0,
        package_content = [Documents_or_Books], -- TODO: get this dynamically
        reference_id = orderId
      }
  where
    mkLocationDetails :: PickupOrDrop -> Flow LocationDetails
    mkLocationDetails PickupOrDrop {..} = do
      (CoreLoc.GPS lat lon) <- CoreLoc._gps _location & fromMaybeM500' "LAT_LON_NOT_FOUND"
      lat' <- readCoord lat
      lon' <- readCoord lon
      address <- CoreLoc._address _location & fromMaybeM500' "ADDRESS_NOT_FOUND"
      return $
        LocationDetails
          { lat = lat',
            lng = lon',
            address =
              Address
                { apartment_address = Just (fromMaybe "" (CoreAddr._door address) <> " " <> CoreAddr._name address <> " " <> fromMaybe "" (CoreAddr._building address)),
                  street_address_1 = fromMaybe "" (CoreAddr._street address),
                  street_address_2 = "",
                  landmark = Nothing,
                  city = Just $ CoreAddr._city address,
                  state = CoreAddr._state address,
                  pincode = Just $ CoreAddr._area_code address,
                  country = Just $ CoreAddr._country address
                }
          }

    mkPersonDetails :: PickupOrDrop -> Flow PersonDetails
    mkPersonDetails PickupOrDrop {..} = do
      phone <- headMaybe (_poc ^. #phones) & fromMaybeM500' "PERSON_PHONENUMBER_NOT_FOUND"
      return $
        PersonDetails
          { name = getName (_poc ^. #name),
            phone_number = phone
          }

    getName :: Name -> Text
    getName Name {..} =
      let def = maybe "" (" " <>)
       in def _honorific_prefix
            <> def _honorific_suffix
            <> _given_name
            <> def _additional_name
            <> def _family_name

mkOnConfirmReq :: Context -> Order -> Flow OnConfirmReq
mkOnConfirmReq context order = do
  return $
    CallbackReq
      { context = context & #_action .~ "on_confirm",
        contents = Right $ ConfirmResMessage order
      }

mkOnConfirmErrReq :: Context -> Error -> Flow OnConfirmReq
mkOnConfirmErrReq context Error {..} = do
  return $
    CallbackReq
      { context = context & #_action .~ "on_confirm",
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

mkOnUpdateErrReq :: Context -> Flow OnUpdateReq
mkOnUpdateErrReq context = do
  return $
    CallbackReq
      { context = context & #_action .~ "on_update",
        contents = Left mkError
      }
  where
    mkError =
      Err.Error
        { _type = "DOMAIN-ERROR",
          _code = "FMD000",
          _path = Nothing,
          _message = Just "UPDATE_NOT_SUPPORTED"
        }

-- TODO: replace this with proper err logging for forked threads
fromMaybeM500' :: BSL.ByteString -> Maybe a -> Flow a
fromMaybeM500' errMsg m = do
  when
    (isNothing m)
    (L.logError @Text "Error" (DT.decodeUtf8 $ BSL.toStrict errMsg))
  fromMaybeM500 errMsg m
