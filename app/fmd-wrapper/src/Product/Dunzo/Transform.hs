{-# LANGUAGE OverloadedLabels #-}

module Product.Dunzo.Transform where

import Beckn.Types.Amount
import Beckn.Types.App
import Beckn.Types.Common
import Control.Lens (element, (?~))
import Control.Lens.Prism (_Just)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (State, drop)
import ExternalAPI.Dunzo.Types
import Servant.Client (BaseUrl (..))
import qualified Types.Beckn.API.Search as SearchAPI
import qualified Types.Beckn.API.Track as TrackAPI
import qualified Types.Beckn.API.Types as API
import Types.Beckn.Catalog (Catalog (..))
import Types.Beckn.Category (Category (..))
import Types.Beckn.Contact (Contact)
import Types.Beckn.Context (Context (..))
import Types.Beckn.DecimalValue (DecimalValue (..), convertAmountToDecimalValue)
import Types.Beckn.Descriptor (emptyDescriptor)
import Types.Beckn.Gps (Gps (..))
import Types.Beckn.Item (Item (..))
import Types.Beckn.Location (Location)
import Types.Beckn.Order (Order (..))
import Types.Beckn.Payment (Params (..), Payment (..), PaymentType (..))
import Types.Beckn.Person (Person)
import Types.Beckn.Price (Price (..))
import Types.Beckn.Provider (Provider (..))
import Types.Beckn.Tracking (Tracking (..))
import Types.Error
import Types.Storage.Organization (Organization)
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
updateBppUri Context {..} bpNwAddress =
  Context
    { bpp_uri = Just bpNwAddress,
      bpp_id = Just . T.pack $ baseUrlHost bpNwAddress,
      ..
    }

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
    & #fulfillment . #id ?~ status.task_id.getTaskId

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

mkCreateTaskReq :: MonadFlow m => Text -> Order -> m CreateTaskReq
mkCreateTaskReq orderId order = do
  pickUpLoc <- order ^? #fulfillment . #start . _Just . #location . _Just & fromMaybeM (InvalidRequest "Pick up location not specified.")
  deliveryLoc <- order ^? #fulfillment . #end . _Just . #location . _Just & fromMaybeM (InvalidRequest "Delivery location not specified.")
  packageCatId <-
    case order.items of
      [orderItem] -> pure orderItem.id
      _ -> throwError $ InvalidRequest "Exactly one order item expected."
  pickupDet <- mkLocationDetails pickUpLoc
  dropDet <- mkLocationDetails deliveryLoc
  pickUpPerson <- order ^? #fulfillment . #start . _Just . #person . _Just & fromMaybeM (InvalidRequest "Sending person not specified.")
  pickUpContact <- order ^? #fulfillment . #start . _Just . #contact . _Just & fromMaybeM (InvalidRequest "Sending person contact not specified.")
  recievingPerson <- order ^? #fulfillment . #end . _Just . #person . _Just & fromMaybeM (InvalidRequest "Recieving person not specified.")
  recievingContact <- order ^? #fulfillment . #end . _Just . #contact . _Just & fromMaybeM (InvalidRequest "Recieving person contact not specified.")
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
        special_instructions = joinInstructions pickupIntructions dropIntructions,
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
      pincode <- address.area_code & fromMaybeErr "AREA_CODE_NOT_FOUND" (Just CORE003)
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
                  pincode = Just pincode,
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

    joinInstructions pickupInstructions dropInstructions =
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

mkPayment :: Float -> Payment
mkPayment estimated_price =
  Payment
    { uri = Nothing,
      tl_method = Nothing,
      params =
        Just
          Params
            { transaction_id = Nothing,
              transaction_status = Nothing,
              amount = Just . DecimalValue $ show estimated_price,
              currency = "INR",
              additional = HMS.empty
            },
      _type = Just POST_FULFILLMENT,
      status = Nothing,
      time = Nothing
    }
