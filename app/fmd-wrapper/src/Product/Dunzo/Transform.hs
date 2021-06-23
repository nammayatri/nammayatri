{-# LANGUAGE OverloadedLabels #-}

module Product.Dunzo.Transform where

import Beckn.Types.Amount
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Storage.Organization (Organization)
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
import qualified Types.Beckn.API.Track as TrackAPI
import qualified Types.Beckn.API.Types as API
import Types.Beckn.Catalog (Catalog (..))
import Types.Beckn.Category (Category (..))
import Types.Beckn.Contact (Contact)
import Types.Beckn.Context (Context (..))
import Types.Beckn.DecimalValue (DecimalValue (..), convertAmountToDecimalValue)
import Types.Beckn.Descriptor (emptyDescriptor)
import Types.Beckn.Duration (Duration (..))
import Types.Beckn.Fulfillment (Fulfillment (..), FulfillmentDetails (..))
import Types.Beckn.Gps (Gps (..))
import Types.Beckn.Item (Item (..))
import Types.Beckn.ItemQuantity (emptyItemQuantity)
import Types.Beckn.Location (Location)
import Types.Beckn.Order (IdAndLocations (..), Order (..), OrderItem (..))
import Types.Beckn.Payment (Params (..), Payment (..), PaymentType (..))
import Types.Beckn.Person (Person)
import Types.Beckn.Price (Price (..))
import Types.Beckn.Provider (Provider (..))
import Types.Beckn.Quotation (Quotation (..))
import Types.Beckn.Time (Time (..))
import Types.Beckn.Tracking (Tracking (..))
import Types.Error
import Types.Wrapper
import Utils.Common

dunzoServiceCategoryId :: Text
dunzoServiceCategoryId = "1"

getDzBAPCreds :: MonadFlow m => Organization -> m DzBAConfig
getDzBAPCreds = getClientConfig

mkQuoteReqFromSearch :: MonadFlow m => API.BecknReq SearchAPI.SearchIntent -> m QuoteReq
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

mkQuoteReqFromInitOrder :: MonadFlow m => InitAPI.InitOrder -> m QuoteReq
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

readCoord :: MonadFlow m => Text -> m Double
readCoord text = do
  readMaybe (T.unpack text)
    & fromMaybeM (InvalidRequest "Location read error.")

mkOnSearchCatalog :: QuoteRes -> SearchAPI.OnSearchCatalog
mkOnSearchCatalog res@QuoteRes {..} =
  SearchAPI.OnSearchCatalog catalog
  where
    catalog =
      Catalog
        { bpp_descriptor = Nothing,
          bpp_categories = Nothing,
          bpp_fulfillments = Nothing,
          bpp_payments = Nothing,
          bpp_offers = Nothing,
          bpp_providers =
            Just
              [ Provider
                  { id = Nothing, -- TBD: https://docs.google.com/document/d/1EqI0lpOXpIdy8uLOZbRevwqr25sn6_lKqLlgWPN1kTs/
                    descriptor =
                      Just $
                        emptyDescriptor
                          & #name ?~ "Dunzo Digital Private Limited",
                    time = Nothing,
                    categories =
                      Just
                        [ Category
                            { id = Just dunzoServiceCategoryId,
                              parent_category_id = Nothing,
                              descriptor =
                                Just $
                                  emptyDescriptor
                                    & #name ?~ "Pickup and drop"
                                    & #code ?~ "pickup_drop",
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

mkSearchItem :: Integer -> PackageContent -> QuoteRes -> Item
mkSearchItem index packageContent QuoteRes {..} =
  Item
    { id = Just $ show index,
      parent_item_id = Nothing,
      descriptor =
        Just $
          emptyDescriptor
            & #name ?~ packageContent.content
            & #code ?~ packageContent.content,
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
      Price
        { currency = Just "INR",
          value = Nothing,
          estimated_value = Just value,
          computed_value = Nothing,
          listed_value = Nothing,
          offered_value = Nothing,
          minimum_value = Nothing,
          maximum_value = Nothing
        }
    value = convertAmountToDecimalValue (Amount $ toRational estimated_price)

mkOnInitMessage :: MonadFlow m => Integer -> InitAPI.InitOrder -> QuoteRes -> m InitAPI.Initialized
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
            Fulfillment
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
            Quotation
              { price = Just price,
                breakup = Nothing,
                ttl = Just . Duration . T.pack $ iso8601Show validTill
              },
        payment = Just $ mkPayment estimated_price
      }
  where
    getItem [item] = pure item
    getItem _ = throwError $ InvalidRequest "Exactly 1 order item expected."
    price = mkPrice estimated_price
    mkPrice estimatedPrice =
      Price
        { currency = Just "INR",
          value = Nothing,
          estimated_value = Just $ convertAmountToDecimalValue $ Amount $ toRational estimatedPrice,
          computed_value = Nothing,
          listed_value = Nothing,
          offered_value = Nothing,
          minimum_value = Nothing,
          maximum_value = Nothing
        }

mkOrderFromInititialized :: MonadFlow m => InitAPI.Initialized -> UTCTime -> m Order
mkOrderFromInititialized initialized now = do
  orderItems <- initialized.items & fromMaybeM (InternalError "OrderItems Initialized convertation error.")
  orderItem <- getItem orderItems
  billing <- initialized.billing & fromMaybeM (InternalError "Billing Initialized convertation error.")
  fulfillment <- initialized.fulfillment & fromMaybeM (InternalError "Fulfilmment Initialized convertation error.")
  quote <- initialized.quote & fromMaybeM (InternalError "Quotation Initialized convertation error.")
  payment <- initialized.payment & fromMaybeM (InternalError "Payment Initialized convertation error.")
  pure
    Order
      { id = Nothing,
        state = Nothing,
        provider =
          IdAndLocations
            { id = "Dunzo",
              locations = []
            },
        items = [OrderItem orderItem.id emptyItemQuantity],
        add_ons = [],
        offers = [],
        billing = billing,
        fulfillment = fulfillment,
        quote = quote,
        payment = payment,
        created_at = Just now,
        updated_at = Just now
      }
  where
    getItem [item] = pure item
    getItem _ = throwError $ InternalError "OrderItem Initialized convertation error."

mkOnStatusMessage :: MonadFlow m => Order -> TaskStatus -> m API.OrderObject
mkOnStatusMessage order status = do
  now <- getCurrentTime
  return . API.OrderObject $ updateOrder Nothing now order status

updateOrder :: Maybe Text -> UTCTime -> Order -> TaskStatus -> Order
updateOrder mbOrderId cTime order status = do
  -- TODO: this assumes that there is one task per order
  let orderState = mapTaskStateToOrderState (status.state)
  let mbPayment = mkPayment <$> status.estimated_price
  order & #id .~ mbOrderId
    & #state ?~ orderState
    & #updated_at ?~ cTime
    & #payment .~ fromMaybe order.payment mbPayment

mkOnTrackMessage :: Maybe BaseUrl -> TrackAPI.OnTrackInfo
mkOnTrackMessage mbTrackingUrl = TrackAPI.OnTrackInfo tracking
  where
    tracking =
      Tracking
        { tl_method = Nothing,
          url = mbTrackingUrl,
          status = Nothing
        }

cancelOrder :: Order -> Order
cancelOrder o =
  o & #state ?~ "CANCELLED"

mkCreateTaskReq :: MonadFlow m => Order -> m CreateTaskReq
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
  let pickupIntructions = formatInstructions "pickup" =<< order ^? #fulfillment . #start . _Just . #instructions
  let dropIntructions = formatInstructions "drop" =<< order ^? #fulfillment . #end . _Just . #instructions
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
    mkLocationDetails :: MonadFlow m => Location -> m LocationDetails
    mkLocationDetails location = do
      -- FIXME: Much of these can be optional I'm pretty sure.
      (Gps lat lon) <- location.gps & fromMaybeErr "LAT_LON_NOT_FOUND" (Just CORE003)
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

    mkPersonDetails :: MonadFlow m => Person -> Contact -> m PersonDetails
    mkPersonDetails person contact = do
      phone <- contact.phone & fromMaybeErr "PERSON_PHONENUMBER_NOT_FOUND" (Just CORE003)
      return $
        PersonDetails
          { name = show person.name,
            phone_number = phone
          }

    formatInstructions tag descriptor =
      descriptor
        >>= (.name)
        >>= Just . ((tag <> ": ") <>)

    joinInstructions orderId pickupInstructions dropInstructions =
      let orderMsg = "Order " <> orderId
       in case (pickupInstructions, dropInstructions) of
            (Just pickupInst, Just dropInst) -> Just $ orderMsg <> ": " <> pickupInst <> " and " <> dropInst
            (Nothing, Just dropInst) -> Just $ orderMsg <> ": " <> dropInst
            (Just pickupInst, Nothing) -> Just $ orderMsg <> ": " <> pickupInst
            _ -> Just orderMsg

mapTaskStateToOrderState :: TaskState -> Text
mapTaskStateToOrderState s =
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

updateOrderEta :: MonadFlow m => FulfillmentDetails -> FulfillmentDetails -> UTCTime -> Eta -> m (FulfillmentDetails, FulfillmentDetails)
updateOrderEta startInfo endInfo now eta = do
  let pickupEta = calcEta now <$> eta.pickup <|> (startInfo.time >>= (.timestamp))
  let dropEta = calcEta now eta.dropoff
  let pickupEtaTime = mkTimeObject <$> pickupEta
  let dropEtaTime = mkTimeObject dropEta
  return (startInfo & #time .~ pickupEtaTime, endInfo & #time ?~ dropEtaTime)
  where
    mkTimeObject time =
      Time
        { label = Nothing,
          timestamp = Just time,
          duration = Nothing,
          range = Nothing,
          days = Nothing
        }

mkPayment :: Float -> Payment
mkPayment estimated_price =
  Payment
    { uri = Nothing,
      tl_method = Nothing,
      params =
        Just
          Params
            { transaction_id = Nothing,
              amount = Just . DecimalValue $ show estimated_price,
              additional = HMS.empty
            },
      payee = Nothing,
      _type = Just PRE_FULFILLMENT,
      status = Nothing,
      time = Nothing
    }

calcEta :: UTCTime -> Float -> UTCTime
calcEta now diffInMinutes = addUTCTime (fromRational $ toRational (diffInMinutes * 60.0)) now
