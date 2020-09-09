{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Dunzo.Transform where

import App.Types
import Beckn.Types.API.Callback
import Beckn.Types.Common
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
import Beckn.Types.Core.Option
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
import Beckn.Types.FMD.Task hiding (TaskState)
import qualified Beckn.Types.FMD.Task as Beckn (TaskState (..))
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

mkOnSearchErrReq :: Context -> Error -> OnSearchReq
mkOnSearchErrReq context err = do
  CallbackReq
    { context = context & #_action .~ "on_search",
      contents = Left $ toBeckn err
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

updateVersions :: Context -> Flow Context
updateVersions Context {..} = do
  env <- ask
  return $
    Context
      { _core_version = Just $ coreVersion env,
        _domain_version = Just $ domainVersion env,
        ..
      }

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
    descriptor = Descriptor n n n n n n n n
    n = Nothing

mkQuote :: QuoteRes -> Flow Quotation
mkQuote QuoteRes {..} = do
  qid <- generateGUID
  return $ Quotation {_id = qid, _price = Just price, _ttl = Nothing, _breakup = Nothing}
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

mkOnSelectMessage :: Order -> QuoteRes -> Flow OnSelectMessage
mkOnSelectMessage order res@QuoteRes {..} = do
  quote <- mkQuote res
  task <- updateTaskEta (head $ order ^. #_tasks) eta
  let order' = order & #_tasks .~ [task]
  return $ OnSelectMessage order' quote

mkOnSelectReq :: Context -> OnSelectMessage -> OnSelectReq
mkOnSelectReq context msg =
  CallbackReq
    { context = context & #_action .~ "on_select",
      contents = Right msg
    }

mkOnSelectErrReq :: Context -> Error -> OnSelectReq
mkOnSelectErrReq context err =
  CallbackReq
    { context = context & #_action .~ "on_select",
      contents = Left $ toBeckn err
    }

mkOnInitMessage :: Text -> Order -> DunzoConfig -> InitReq -> QuoteRes -> Flow InitOrder
mkOnInitMessage orderId order conf req QuoteRes {..} = do
  task <- updateTaskEta (head $ order ^. #_tasks) eta
  return $
    InitOrder $
      order & #_id ?~ orderId
        & #_payment ?~ mkPayment conf estimated_price
        & #_billing .~ billing
        & #_tasks .~ [task]
  where
    billing = req ^. #message . #order . #_billing

mkOnInitReq :: Context -> InitOrder -> OnInitReq
mkOnInitReq context msg =
  CallbackReq
    { context = context & #_action .~ "on_init",
      contents = Right msg
    }

mkOnInitErrReq :: Context -> Error -> OnInitReq
mkOnInitErrReq context err =
  CallbackReq
    { context = context & #_action .~ "on_init",
      contents = Left $ toBeckn err
    }

{-# ANN mkOnStatusMessage ("HLint: ignore Use <$>" :: String) #-}
mkOnStatusMessage :: Text -> Order -> DunzoConfig -> TaskStatus -> Flow StatusResMessage
mkOnStatusMessage orgName order conf status = do
  now <- getCurrTime
  return $ StatusResMessage (updateOrder orgName now order conf status)

updateOrder :: Text -> UTCTime -> Order -> DunzoConfig -> TaskStatus -> Order
updateOrder orgName cTime order conf status = do
  -- TODO: this assumes that there is one task per order
  let orderState = mapTaskStateToOrderState (status ^. #state)
  let cancellationReasonIfAny =
        case status ^. #state of
          CANCELLED -> Just [Option "1" (Descriptor (Just "User cancelled") n n n n n n n)]
          RUNNER_CANCELLED -> Just [Option "1" (Descriptor (Just "Agent cancelled") n n n n n n n)]
          _ -> Nothing
  let payment = mkPayment conf <$> status ^. #estimated_price
  order & #_state ?~ orderState
    & #_updated_at ?~ cTime
    & #_payment .~ (payment <|> order ^. #_payment)
    & #_cancellation_reasons .~ (cancellationReasonIfAny <|> order ^. #_cancellation_reasons)
    & #_tasks .~ (updateTask <$> (order ^. #_tasks))
  where
    updateTask task = do
      let taskState = mapTaskState (status ^. #state)
      let eta = status ^. #eta
      let pickup = task ^. #_pickup
      let drop = task ^. #_drop

      let pickupEta = addEta . (^. #pickup) <$> eta
      let dropEta = addEta . (^. #dropoff) <$> eta
      let pickup' = pickup & #_time .~ (pickupEta <|> pickup ^. #_time)
      let drop' = drop & #_time .~ (dropEta <|> drop ^. #_time)

      task & #_agent .~ (getAgent <$> status ^. #runner)
        & #_state .~ (taskState <|> task ^. #_state)
        & #_updated_at ?~ cTime
        & #_pickup .~ pickup'
        & #_drop .~ drop'

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

    addEta duration = addUTCTime (fromInteger $ duration * 60 :: NominalDiffTime) cTime

mkOnStatusReq :: Context -> StatusResMessage -> Flow OnStatusReq
mkOnStatusReq context msg =
  return $
    CallbackReq
      { context = context & #_action .~ "on_status",
        contents = Right msg
      }

mkOnStatusErrReq :: Context -> Error -> OnStatusReq
mkOnStatusErrReq context err =
  CallbackReq
    { context = context & #_action .~ "on_status",
      contents = Left $ toBeckn err
    }

mkOnTrackErrReq :: Context -> OnTrackReq
mkOnTrackErrReq context = do
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
mkOnCancelReq context o = do
  let order =
        o & #_state ?~ "CANCELLED"
          & #_cancellation_reasons ?~ [Option "1" (Descriptor (Just "User cancelled") n n n n n n n)]
  return $
    CallbackReq
      { context = context & #_action .~ "on_cancel",
        contents = Right (CancelResMessage order)
      }
  where
    n = Nothing

mkOnCancelErrReq :: Context -> Error -> OnCancelReq
mkOnCancelErrReq context err =
  CallbackReq
    { context = context & #_action .~ "on_cancel",
      contents = Left $ toBeckn err
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
        reference_id = order ^. #_prev_order_id
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

mkOnConfirmErrReq :: Context -> Error -> OnConfirmReq
mkOnConfirmErrReq context err =
  CallbackReq
    { context = context & #_action .~ "on_confirm",
      contents = Left $ toBeckn err
    }

mkOnUpdateErrReq :: Context -> OnUpdateReq
mkOnUpdateErrReq context = do
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

mapTaskState :: TaskState -> Maybe Beckn.TaskState
mapTaskState s = case s of
  CREATED -> Just Beckn.SEARCHING_FOR_FMD_AGENT
  QUEUED -> Just Beckn.SEARCHING_FOR_FMD_AGENT
  RUNNER_ACCEPTED -> Just Beckn.ASSIGNED_AGENT
  REACHED_FOR_PICKUP -> Just Beckn.AT_PICKUP_LOCATION
  PICKUP_COMPLETE -> Just Beckn.PICKED_UP_PACKAGE
  STARTED_FOR_DELIVERY -> Just Beckn.EN_ROUTE_TO_DROP
  REACHED_FOR_DELIVERY -> Just Beckn.AT_DROP_LOCATION
  DELIVERED -> Just Beckn.DROPPED_PACKAGE
  CANCELLED -> Nothing
  RUNNER_CANCELLED -> Nothing

mapTaskStateToOrderState :: TaskState -> Text
mapTaskStateToOrderState s = case s of
  CREATED -> "ACTIVE"
  QUEUED -> "ACTIVE"
  RUNNER_ACCEPTED -> "ACTIVE"
  REACHED_FOR_PICKUP -> "ACTIVE"
  PICKUP_COMPLETE -> "ACTIVE"
  STARTED_FOR_DELIVERY -> "ACTIVE"
  REACHED_FOR_DELIVERY -> "ACTIVE"
  DELIVERED -> "COMPLETED"
  CANCELLED -> "CANCELLED"
  RUNNER_CANCELLED -> "CANCELLED"

updateTaskEta :: Task -> Eta -> Flow Task
updateTaskEta task eta = do
  now <- getCurrTime
  let pickup = task ^. #_pickup
  let drop = task ^. #_drop

  let pickupEta = addUTCTime (fromInteger $ (eta ^. #pickup) * 60 :: NominalDiffTime) now
  let dropEta = addUTCTime (fromInteger $ (eta ^. #dropoff) * 60 :: NominalDiffTime) now
  let pickup' = pickup & #_time ?~ pickupEta
  let drop' = drop & #_time ?~ dropEta
  return $
    task & #_pickup .~ pickup'
      & #_drop .~ drop'

mkPayment :: DunzoConfig -> Float -> Payment
mkPayment conf estimated_price =
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
  where
    price =
      MonetaryValue
        { _currency = "INR",
          _value = convertAmountToDecimalValue $ Amount $ toRational estimated_price
        }
