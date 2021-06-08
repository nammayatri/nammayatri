{-# LANGUAGE OverloadedLabels #-}

module Product.Dunzo.Transform where

import App.Types
import Beckn.Types.Amount
import Beckn.Types.App
import Beckn.Types.Common
import qualified Beckn.Types.Core.Migration.Catalog as M.Catalog
import qualified Beckn.Types.Core.Migration.Category as M.Category
import qualified Beckn.Types.Core.Migration.Context as M.Context
import qualified Beckn.Types.Core.Migration.DecimalValue as M.DecimalValue
import qualified Beckn.Types.Core.Migration.Descriptor as M.Descriptor
import qualified Beckn.Types.Core.Migration.Item as M.Item
import qualified Beckn.Types.Core.Migration.Price as M.Price
import qualified Beckn.Types.Core.Migration.Provider as M.Provider
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.JSON
import Control.Lens (element, (?~))
import Control.Lens.Prism (_Just)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime)
import EulerHS.Prelude hiding (State, drop)
import ExternalAPI.Dunzo.Types
import Types.Beckn.API.Init
import qualified Types.Beckn.API.Search as SearchAPI
import Types.Beckn.API.Select
import Types.Beckn.API.Status
import Types.Beckn.API.Track
import qualified Types.Beckn.API.Types as API
import qualified Types.Beckn.Address as CoreAddr
import Types.Beckn.Context
import Types.Beckn.DecimalValue
import Types.Beckn.Descriptor
import Types.Beckn.FmdOrder
import qualified Types.Beckn.Location as CoreLoc
import Types.Beckn.MonetaryValue
import Types.Beckn.Operator
import Types.Beckn.Option
import Types.Beckn.Package
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
                        M.Descriptor.Descriptor
                          { name = Just "Dunzo Digital Private Limited",
                            code = Nothing,
                            symbol = Nothing,
                            short_desc = Nothing,
                            long_desc = Nothing,
                            images = Nothing,
                            audio = Nothing,
                            _3d_render = Nothing
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

mkOnInitMessage :: Text -> Integer -> Order -> PaymentEndpoint -> InitReq -> QuoteRes -> Flow InitOrder
mkOnInitMessage orderId quotationTTLinMin order payee req QuoteRes {..} = do
  task <- updateTaskEta (head $ order.tasks) eta
  now <- getCurrentTime
  let validTill = addUTCTime (fromInteger (quotationTTLinMin * 60)) now
  quotation <- (order.quotation) & fromMaybeM (InternalError "Invalid order, no quotation.")
  return $
    InitOrder $
      order & #id ?~ orderId
        & #quotation ?~ (quotation & #ttl ?~ validTill)
        & #payment ?~ mkPayment payee estimated_price
        & #billing .~ billing
        & #tasks .~ [task]
  where
    billing = req.message.order.billing

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

mkCreateTaskReq :: Order -> Flow CreateTaskReq
mkCreateTaskReq order = do
  orderId <- order.id & fromMaybeErr "ORDER_ID_MISSING" (Just CORE003)
  let [task] = order.tasks
  let pickup = task.pickup
  let drop = task.drop
  let package = task.package
  pickupDet <- mkLocationDetails pickup
  dropDet <- mkLocationDetails drop
  senderDet <- mkPersonDetails pickup
  receiverDet <- mkPersonDetails drop
  let pickupIntructions = formatInstructions "pickup" =<< pickup.instructions
  let dropIntructions = formatInstructions "drop" =<< drop.instructions
  let mTotalValue = (\(Amount a) -> fromRational a) <$> getPackageValue package
  packageContent <- do
    (categoryId :: Int) <- fromMaybeErr "INVALID_CATEGORY_ID" (Just CORE003) ((readMaybe . T.unpack) =<< package.package_category_id)
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
        package_approx_value = mTotalValue,
        package_content = [packageContent],
        reference_id = order.prev_order_id
      }
  where
    mkLocationDetails :: PickupOrDrop -> Flow LocationDetails
    mkLocationDetails PickupOrDrop {..} = do
      (CoreLoc.GPS lat lon) <- CoreLoc.gps location & fromMaybeErr "LAT_LON_NOT_FOUND" (Just CORE003)
      lat' <- readCoord lat
      lon' <- readCoord lon
      address <- CoreLoc.address location & fromMaybeErr "ADDRESS_NOT_FOUND" (Just CORE003)
      return $
        LocationDetails
          { lat = lat',
            lng = lon',
            address =
              Address
                { apartment_address = Just (CoreAddr.door address <> maybe "" (" " <>) (CoreAddr.name address) <> maybe "" (" " <>) (CoreAddr.building address)),
                  street_address_1 = CoreAddr.street address,
                  street_address_2 = "",
                  landmark = Nothing,
                  city = Just $ CoreAddr.city address,
                  state = CoreAddr.state address,
                  pincode = Just $ CoreAddr.area_code address,
                  country = Just $ CoreAddr.country address
                }
          }

    mkPersonDetails :: PickupOrDrop -> Flow PersonDetails
    mkPersonDetails PickupOrDrop {..} = do
      phone <- listToMaybe (poc.phones) & fromMaybeErr "PERSON_PHONENUMBER_NOT_FOUND" (Just CORE003)
      return $
        PersonDetails
          { name = getName (poc.name),
            phone_number = phone
          }

    getName :: Name -> Text
    getName Name {..} =
      let def = maybe "" (" " <>)
       in def honorific_prefix
            <> def honorific_suffix
            <> given_name
            <> def additional_name
            <> def family_name

    getPackageValue :: Package -> Maybe Amount
    getPackageValue package = do
      let mprice = package.price
      case mprice of
        Nothing -> Nothing
        Just price -> convertDecimalValueToAmount =<< (price.value)

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

calcEta :: UTCTime -> Float -> UTCTime
calcEta now diffInMinutes = addUTCTime (fromRational $ toRational (diffInMinutes * 60.0)) now
