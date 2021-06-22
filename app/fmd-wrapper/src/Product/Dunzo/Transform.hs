{-# LANGUAGE OverloadedLabels #-}

module Product.Dunzo.Transform where

import App.Types
import Beckn.Types.Amount
import Beckn.Types.App
import Beckn.Types.Common
import qualified Beckn.Types.Core.Migration.Catalog as M.Catalog
import qualified Beckn.Types.Core.Migration.Category as M.Category
import qualified Beckn.Types.Core.Migration.Contact as M.Contact
import qualified Beckn.Types.Core.Migration.Context as M.Context
import qualified Beckn.Types.Core.Migration.DecimalValue as M.DecimalValue
import qualified Beckn.Types.Core.Migration.Descriptor as M.Descriptor
import qualified Beckn.Types.Core.Migration.Duration as M.Duration
import qualified Beckn.Types.Core.Migration.Fulfillment as M.Fulfillment
import qualified Beckn.Types.Core.Migration.Gps as M.Gps
import qualified Beckn.Types.Core.Migration.Item as M.Item
import qualified Beckn.Types.Core.Migration.Location as M.Location
import qualified Beckn.Types.Core.Migration.Order as M.Order
import qualified Beckn.Types.Core.Migration.Payment as M.Payment
import qualified Beckn.Types.Core.Migration.Person as M.Person
import qualified Beckn.Types.Core.Migration.Price as M.Price
import qualified Beckn.Types.Core.Migration.Provider as M.Provider
import qualified Beckn.Types.Core.Migration.Quotation as M.Quotation
import qualified Beckn.Types.Core.Migration.Time as M.Time
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.JSON
import Control.Lens (element, (?~))
import Control.Lens.Prism (_Just)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import EulerHS.Prelude hiding (State, drop)
import ExternalAPI.Dunzo.Types
import Types.Beckn.API.Init
import qualified Types.Beckn.API.Init as InitAPI
import qualified Types.Beckn.API.Search as SearchAPI
import Types.Beckn.API.Select
import Types.Beckn.API.Status
import Types.Beckn.API.Track
import qualified Types.Beckn.API.Types as API
import Types.Beckn.Context
import Types.Beckn.DecimalValue
import Types.Beckn.Descriptor
import Types.Beckn.FmdOrder
import Types.Beckn.MonetaryValue
import Types.Beckn.Operator
import Types.Beckn.Option
import Types.Beckn.Payment
import Types.Beckn.PaymentEndpoint
import Types.Beckn.Person
import Types.Beckn.Price
import Types.Beckn.Quotation
import Types.Beckn.State
import Types.Beckn.Task hiding (TaskState)
import qualified Types.Beckn.Task as Beckn (TaskState (..))
import Types.Beckn.Tracking
import Types.Error
import Types.Wrapper
import Utils.Common

dunzoServiceCategoryId :: Text
dunzoServiceCategoryId = "1"

getDzBAPCreds :: Organization -> Flow DzBAConfig
getDzBAPCreds = getClientConfig

mkQuoteReqFromSearch :: API.BecknReq SearchAPI.SearchIntent -> Flow QuoteReq
mkQuoteReqFromSearch API.BecknReq {..} = do
  let intent = message.intent
  let mbPickupGps = intent ^? #fulfillment . _Just . #start . _Just . #location . _Just . #gps . _Just
  let mbDropGps = intent ^? #fulfillment . _Just . #end . _Just . #location . _Just . #gps . _Just
  case (mbPickupGps, mbDropGps) of
    (Just pickupGps, Just dropGps) -> do
      return $
        QuoteReq
          { pickup_lat = pickupGps.lat,
            pickup_lng = pickupGps.lon,
            drop_lat = dropGps.lat,
            drop_lng = dropGps.lon,
            category_id = "pickup_drop"
          }
    (Just _, Nothing) -> dropLocationNotFound
    _ -> pickupLocationNotFound
  where
    pickupLocationNotFound = throwError $ InvalidRequest "Pickup location not found."
    dropLocationNotFound = throwError $ InvalidRequest "Drop location not found."

mkQuoteReqFromSelect :: SelectReq -> Flow QuoteReq
mkQuoteReqFromSelect SelectReq {..} = do
  let tasks = message.order.tasks
      task = head tasks
      pickup = task.pickup.location
      drop = task.drop.location
      pgps = fromJust pickup.gps
      dgps = fromJust drop.gps
  plat <- readCoord pgps.lat
  plon <- readCoord pgps.lon
  dlat <- readCoord dgps.lat
  dlon <- readCoord dgps.lon
  return $
    QuoteReq
      { pickup_lat = plat,
        pickup_lng = plon,
        drop_lat = dlat,
        drop_lng = dlon,
        category_id = "pickup_drop"
      }

mkQuoteReqFromInitOrder :: InitAPI.InitOrder -> Flow QuoteReq
mkQuoteReqFromInitOrder order = do
  let mbPickupGps = order ^? #fulfillment . #start . _Just . #location . _Just . #gps . _Just
  let mbDropGps = order ^? #fulfillment . #end . _Just . #location . _Just . #gps . _Just
  case (mbPickupGps, mbDropGps) of
    (Just pickupGps, Just dropGps) -> do
      return $
        QuoteReq
          { pickup_lat = pickupGps.lat,
            pickup_lng = pickupGps.lon,
            drop_lat = dropGps.lat,
            drop_lng = dropGps.lon,
            category_id = "pickup_drop"
          }
    (Just _, Nothing) -> dropLocationNotFound
    _ -> pickupLocationNotFound
  where
    pickupLocationNotFound = throwError $ InvalidRequest "Pickup location not found."
    dropLocationNotFound = throwError $ InvalidRequest "Drop location not found."

readCoord :: Text -> Flow Double
readCoord text = do
  readMaybe (T.unpack text)
    & fromMaybeM (InvalidRequest "Location read error.")

mkOnSearchCatalog :: QuoteRes -> SearchAPI.OnSearchCatalog
mkOnSearchCatalog res@QuoteRes {..} =
  SearchAPI.OnSearchCatalog catalog
  where
    catalog =
      M.Catalog.Catalog
        { bpp_descriptor = Nothing,
          bpp_categories = Nothing,
          bpp_fulfillments = Nothing,
          bpp_payments = Nothing,
          bpp_offers = Nothing,
          bpp_providers =
            Just
              [ M.Provider.Provider
                  { id = Nothing, -- TBD: https://docs.google.com/document/d/1EqI0lpOXpIdy8uLOZbRevwqr25sn6_lKqLlgWPN1kTs/
                    descriptor =
                      Just
                        M.Descriptor.emptyDescriptor
                          { M.Descriptor.name = Just "Dunzo Digital Private Limited"
                          },
                    time = Nothing,
                    categories =
                      Just
                        [ M.Category.Category
                            { id = Just dunzoServiceCategoryId,
                              parent_category_id = Nothing,
                              descriptor =
                                Just
                                  M.Descriptor.emptyDescriptor
                                    { M.Descriptor.name = Just "Pickup and drop",
                                      M.Descriptor.code = Just "pickup_drop"
                                    },
                              time = Nothing,
                              tags = Nothing
                            }
                        ],
                    fulfillments = Nothing,
                    payments = Nothing,
                    locations = Nothing,
                    offers = Nothing,
                    items =
                      Just $
                        foldWIndex
                          (\index acc packageContent -> acc <> [mkSearchItem (index + 1) packageContent res])
                          []
                          dzPackageContentList,
                    exp = Nothing,
                    tags = Nothing
                  }
              ],
          exp = Nothing
        }

updateBppUri :: Context -> BaseUrl -> Context
updateBppUri Context {..} bpNwAddress = Context {bpp_uri = Just bpNwAddress, ..}

updateBppUriMig :: M.Context.Context -> BaseUrl -> M.Context.Context
updateBppUriMig M.Context.Context {..} bpNwAddress = M.Context.Context {bpp_uri = Just bpNwAddress, ..}

mkSearchItem :: Integer -> PackageContent -> QuoteRes -> M.Item.Item
mkSearchItem index packageContent QuoteRes {..} =
  M.Item.Item
    { id = Just $ show index,
      parent_item_id = Nothing,
      descriptor =
        Just $
          M.Descriptor.emptyDescriptor
            { M.Descriptor.name = Just $ packageContent.content,
              M.Descriptor.code = Just $ packageContent.content
            },
      price = Just price,
      category_id = Just dunzoServiceCategoryId,
      location_id = Nothing,
      time = Nothing,
      matched = Nothing,
      related = Nothing,
      recommended = Nothing,
      tags = Nothing
    }
  where
    price =
      M.Price.Price
        { currency = Just "INR",
          value = Nothing,
          estimated_value = Just value,
          computed_value = Nothing,
          listed_value = Nothing,
          offered_value = Nothing,
          minimum_value = Nothing,
          maximum_value = Nothing
        }
    value = M.DecimalValue.convertAmountToDecimalValue (Amount $ toRational estimated_price)

mkQuote :: Integer -> QuoteRes -> Flow Quotation
mkQuote quotationTTLinMin QuoteRes {..} = do
  qid <- generateGUID
  now <- getCurrentTime
  let validTill = addUTCTime (fromInteger (quotationTTLinMin * 60)) now
  return $ Quotation {id = qid, price = Just price, ttl = Just validTill, breakup = Nothing}
  where
    price = mkPrice estimated_price
    mkPrice estimatedPrice =
      Price
        { currency = "INR",
          value = Nothing,
          estimated_value = Just $ convertAmountToDecimalValue $ Amount $ toRational estimatedPrice,
          computed_value = Nothing,
          listed_value = Nothing,
          offered_value = Nothing,
          minimum_value = Nothing,
          maximum_value = Nothing
        }

mkOnSelectOrder :: Order -> Integer -> QuoteRes -> Flow SelectOrder
mkOnSelectOrder order quotationTTLinMin res@QuoteRes {..} = do
  quote <- mkQuote quotationTTLinMin res
  task <- updateTaskEta (head $ order.tasks) eta
  let order' =
        order & #tasks .~ [task]
          & #quotation ?~ quote
  return $ SelectOrder order'

mkOnInitMessage :: Integer -> InitAPI.InitOrder -> QuoteRes -> Flow InitAPI.Initialized
mkOnInitMessage quotationTTLinMin order QuoteRes {..} = do
  orderItem <- getItem order.items -- probably quantity field must be validated
  now <- getCurrentTime

  reqStartInfo <- order.fulfillment.start & fromMaybeM (InvalidRequest "Pickup location not found.")
  reqEndInfo <- order.fulfillment.start & fromMaybeM (InvalidRequest "Drop location not found.")
  (startInfo, endInfo) <- updateOrderEta reqStartInfo reqEndInfo now eta

  let validTill = addUTCTime (fromInteger (quotationTTLinMin * 60)) now
  return $
    InitAPI.Initialized
      { provider = Nothing,
        provider_location = Nothing,
        items = Just [InitAPI.InitOrderItem orderItem.id 1],
        add_ons = Nothing,
        offers = Nothing,
        billing = Just order.billing,
        fulfillment =
          Just $
            M.Fulfillment.Fulfillment
              { id = Nothing,
                _type = Nothing,
                state = Nothing,
                tracking = False,
                customer = Nothing,
                agent = Nothing,
                vehicle = Nothing,
                start = Just startInfo,
                end = Just endInfo,
                purpose = Nothing,
                tags = Nothing
              },
        quote =
          Just $
            M.Quotation.Quotation
              { price = Just price,
                breakup = Nothing,
                ttl = Just . M.Duration.Duration . T.pack $ iso8601Show validTill
              },
        payment = Just $ mkPaymentMig estimated_price
      }
  where
    getItem [item] = pure item
    getItem _ = throwError $ InvalidRequest "Exactly 1 order item expected."
    price = mkPrice estimated_price
    mkPrice estimatedPrice =
      M.Price.Price
        { currency = Just "INR",
          value = Nothing,
          estimated_value = Just $ M.DecimalValue.convertAmountToDecimalValue $ Amount $ toRational estimatedPrice,
          computed_value = Nothing,
          listed_value = Nothing,
          offered_value = Nothing,
          minimum_value = Nothing,
          maximum_value = Nothing
        }

{-# ANN mkOnStatusMessage ("HLint: ignore Use <$>" :: String) #-}
mkOnStatusMessage :: Text -> Order -> PaymentEndpoint -> TaskStatus -> Flow StatusResMessage
mkOnStatusMessage orgName order payee status = do
  now <- getCurrentTime
  return $ StatusResMessage (updateOrder orgName now order payee status)

updateOrder :: Text -> UTCTime -> Order -> PaymentEndpoint -> TaskStatus -> Order
updateOrder orgName cTime order payee status = do
  -- TODO: this assumes that there is one task per order
  let orderState = mapTaskStateToOrderState (status.state)
  let cancellationReasonIfAny =
        case status.state of
          CANCELLED -> Just [Option "1" (withName "User cancelled")]
          RUNNER_CANCELLED -> Just [Option "1" (withName "Agent cancelled")]
          _ -> Nothing
  let payment = mkPayment payee <$> status.estimated_price
  order & #state ?~ orderState
    & #updated_at ?~ cTime
    & #payment .~ (payment <|> order.payment)
    & #cancellation_reasons .~ (cancellationReasonIfAny <|> order.cancellation_reasons)
    & #tasks .~ (updateTask <$> (order.tasks))
  where
    updateTask task = do
      let taskState = mapTaskState (status.state)
      let eta = status.eta
      let pickup = task.pickup
      let drop = task.drop

      let pickupEta = calcEta cTime <$> ((.pickup) =<< eta)
      let dropEta = calcEta cTime . (.dropoff) <$> eta
      let pickup' = pickup & #time .~ (pickupEta <|> pickup.time)
      let drop' = drop & #time .~ (dropEta <|> drop.time)

      task & #agent .~ (getAgent <$> status.runner)
        & #state .~ (taskState <|> task.state)
        & #updated_at ?~ cTime
        & #pickup .~ pickup'
        & #drop .~ drop'

    getAgent runner =
      Operator
        { name = withGivenName $ runner.name,
          image = n,
          dob = n,
          organization_name = Just orgName,
          gender = n,
          email = n,
          phones = [runner.phone_number],
          experience = n
        }

    n = Nothing

updateOrderMig :: UTCTime -> M.Order.Order -> TaskStatus -> M.Order.Order
updateOrderMig cTime order status = do
  -- TODO: this assumes that there is one task per order
  let orderState = mapTaskStateToOrderStateMig (status.state)
  let mbPayment = mkPaymentMig <$> status.estimated_price
  order & #state ?~ orderState
    & #updated_at ?~ cTime
    & #payment .~ fromMaybe order.payment mbPayment

mkOnTrackMessage :: Text -> Maybe Text -> TrackResMessage
mkOnTrackMessage orderId trackingUrl = TrackResMessage tracking orderId
  where
    tracking =
      Tracking
        { url = trackingUrl,
          required_params = Nothing,
          metadata = Nothing
        }

cancelOrder :: Order -> Order
cancelOrder o =
  o & #state ?~ withDescriptor (withCode "CANCELLED")
    & #cancellation_reasons ?~ [Option "1" (withName "User cancelled")]

mkCreateTaskReq :: M.Order.Order -> Flow CreateTaskReq
mkCreateTaskReq order = do
  orderId <- order.id & fromMaybeErr "ORDER_ID_MISSING" (Just CORE003)
  pickUpLoc <- order ^? #fulfillment . #start . _Just . #location . _Just & fromMaybeM (InvalidRequest "Pick up location not specified.")
  deliveryLoc <- order ^? #fulfillment . #end . _Just . #location . _Just & fromMaybeM (InvalidRequest "Delivery location not specified.")
  packageCatId <-
    case order.items of
      [orderItem] -> pure orderItem.id
      _ -> throwError $ InvalidRequest "Exactly one order item expected."
  pickupDet <- mkLocationDetails pickUpLoc
  dropDet <- mkLocationDetails deliveryLoc
  pickUpPerson <- order ^? #fulfillment . #start . _Just . #person & fromMaybeM (InvalidRequest "Sending person not specified.")
  pickUpContact <- order ^? #fulfillment . #start . _Just . #contact & fromMaybeM (InvalidRequest "Sending person contact not specified.")
  recievingPerson <- order ^? #fulfillment . #end . _Just . #person & fromMaybeM (InvalidRequest "Recieving person not specified.")
  recievingContact <- order ^? #fulfillment . #end . _Just . #contact & fromMaybeM (InvalidRequest "Recieving person contact not specified.")
  senderDet <- mkPersonDetails pickUpPerson pickUpContact
  receiverDet <- mkPersonDetails recievingPerson recievingContact
  let pickupIntructions = formatInstructions "pickup" =<< order ^. #fulfillment . #start . _Just . #instructions
  let dropIntructions = formatInstructions "drop" =<< order ^. #fulfillment . #end . _Just . #instructions
  packageContent <- do
    (categoryId :: Int) <- fromMaybeErr "INVALID_CATEGORY_ID" (Just CORE003) (readMaybe $ T.unpack packageCatId)
    -- Category id is the index value of dzPackageContentList
    dzPackageContentList ^? element (categoryId - 1)
      & fromMaybeErr "INVALID_CATEGORY_ID" (Just CORE003)
  return $
    CreateTaskReq
      { request_id = orderId,
        pickup_details = pickupDet,
        drop_details = dropDet,
        sender_details = senderDet,
        receiver_details = receiverDet,
        special_instructions = joinInstructions orderId pickupIntructions dropIntructions,
        package_approx_value = Nothing, -- FIXME. Don't know where BAP can specify this in the new spec.
        package_content = [packageContent],
        reference_id = Nothing
      }
  where
    mkLocationDetails :: M.Location.Location -> Flow LocationDetails
    mkLocationDetails location = do
      -- FIXME: Much of these can be optional I'm pretty sure.
      (M.Gps.Gps lat lon) <- location.gps & fromMaybeErr "LAT_LON_NOT_FOUND" (Just CORE003)
      address <- location.address & fromMaybeErr "ADDRESS_NOT_FOUND" (Just CORE003)
      door <- address.door & fromMaybeErr "DOOR_NOT_FOUND" (Just CORE003)
      street <- address.street & fromMaybeErr "STREET_NOT_FOUND" (Just CORE003)
      city <- address.city & fromMaybeErr "CITY_NOT_FOUND" (Just CORE003)
      state_ <- address.state & fromMaybeErr "STATE_NOT_FOUND" (Just CORE003)
      country <- address.country & fromMaybeErr "COUNTRY_NOT_FOUND" (Just CORE003)
      return $
        LocationDetails
          { lat = lat,
            lng = lon,
            address =
              Address
                { apartment_address = Just (door <> maybe "" (" " <>) address.name <> maybe "" (" " <>) address.building),
                  street_address_1 = street,
                  street_address_2 = "",
                  landmark = Nothing,
                  city = Just city,
                  state = state_,
                  pincode = address.area_code,
                  country = Just country
                }
          }

    mkPersonDetails :: M.Person.Person -> M.Contact.Contact -> Flow PersonDetails
    mkPersonDetails person contact = do
      phone <- contact.phone & fromMaybeErr "PERSON_PHONENUMBER_NOT_FOUND" (Just CORE003)
      return $
        PersonDetails
          { name = show person.name,
            phone_number = phone
          }

    formatInstructions tag descriptors = do
      let insts = mapMaybe (.name) descriptors
      if null insts
        then Nothing
        else Just $ tag <> ": " <> T.intercalate ", " insts

    joinInstructions orderId pickupInstructions dropInstructions =
      let orderMsg = "Order " <> orderId
       in case (pickupInstructions, dropInstructions) of
            (Just pickupInst, Just dropInst) -> Just $ orderMsg <> ": " <> pickupInst <> " and " <> dropInst
            (Nothing, Just dropInst) -> Just $ orderMsg <> ": " <> dropInst
            (Just pickupInst, Nothing) -> Just $ orderMsg <> ": " <> pickupInst
            _ -> Just orderMsg

mapTaskState :: TaskState -> Maybe State
mapTaskState s =
  let mstate = case s of
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
   in toState <$> mstate
  where
    toState taskState =
      withDescriptor $ withCode $ replaceUnderscores $ show taskState

mapTaskStateToOrderState :: TaskState -> State
mapTaskStateToOrderState s = do
  let code = case s of
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
  withDescriptor $ withCode code

mapTaskStateToOrderStateMig :: TaskState -> Text
mapTaskStateToOrderStateMig s =
  case s of
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
  now <- getCurrentTime
  let pickup = task.pickup
  let drop = task.drop

  let pickupEta = calcEta now <$> (eta.pickup) <|> (task.pickup.time)
  let dropEta = calcEta now (eta.dropoff)
  let pickup' = pickup & #time .~ pickupEta
  let drop' = drop & #time ?~ dropEta
  return $
    task & #pickup .~ pickup'
      & #drop .~ drop'

updateOrderEta :: M.Fulfillment.FulfillmentDetails -> M.Fulfillment.FulfillmentDetails -> UTCTime -> Eta -> Flow (M.Fulfillment.FulfillmentDetails, M.Fulfillment.FulfillmentDetails)
updateOrderEta startInfo endInfo now eta = do
  let pickupEta = calcEta now <$> eta.pickup <|> (startInfo.time >>= (.timestamp))
  let dropEta = calcEta now eta.dropoff
  let pickupEtaTime = mkTimeObject <$> pickupEta
  let dropEtaTime = mkTimeObject dropEta
  return (startInfo & #time .~ pickupEtaTime, endInfo & #time ?~ dropEtaTime)
  where
    mkTimeObject time =
      M.Time.Time
        { label = Nothing,
          timestamp = Just time,
          duration = Nothing,
          range = Nothing,
          days = Nothing
        }

mkPayment :: PaymentEndpoint -> Float -> Payment
mkPayment payee estimated_price =
  Payment
    { transaction_id = Nothing,
      _type = Just "PRE-FULFILLMENT",
      payer = Nothing,
      payee = Just payee,
      methods = ["RTGS"],
      amount = price,
      state = Nothing,
      due_date = Nothing,
      duration = Nothing
    }
  where
    price =
      MonetaryValue
        { currency = "INR",
          value = convertAmountToDecimalValue $ Amount $ toRational estimated_price
        }

mkPaymentMig :: Float -> M.Payment.Payment
mkPaymentMig estimated_price =
  M.Payment.Payment
    { uri = Nothing,
      tl_method = Nothing,
      params =
        Just
          M.Payment.Params
            { transaction_id = Nothing,
              amount = Just . M.DecimalValue.DecimalValue $ show estimated_price,
              additional = HMS.empty
            },
      payee = Nothing,
      _type = Just M.Payment.PRE_FULFILLMENT,
      status = Nothing,
      time = Nothing
    }

calcEta :: UTCTime -> Float -> UTCTime
calcEta now diffInMinutes = addUTCTime (fromRational $ toRational (diffInMinutes * 60.0)) now
