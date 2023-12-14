{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnSelect where

import Beckn.ACL.Common
-- import Data.Time.Clock(diffTimeToPicoseconds,secondsToDiffTime)

import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnSelect as OnSelect
import qualified Beckn.Types.Core.Taxi.OnSelect as OnSelect
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Time.LocalTime (CalendarDiffTime, ctTime)
import qualified Domain.Action.Beckn.OnSelect as DOnSelect
import Domain.Types.VehicleVariant
import EulerHS.Prelude (safeHead)
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.TimeRFC339
import Kernel.Utils.Common
import Tools.Error

buildOnSelectReq ::
  HasFlowEnv m r '["coreVersion" ::: Text] =>
  OnSelect.OnSelectReq ->
  m (Maybe DOnSelect.DOnSelectReq)
buildOnSelectReq req = do
  logDebug $ "on_select request: " <> show req
  let context = req.context
  let timestamp = context.timestamp
  validateContext Context.ON_SELECT context
  handleError req.contents $ \message -> do
    providerId <- context.bpp_id & fromMaybeM (InvalidRequest "Missing bpp_id")
    providerUrl <- context.bpp_uri & fromMaybeM (InvalidRequest "Missing bpp_uri")
    let items = message.order.items
    quotesInfo <- traverse (buildQuoteInfo message.order.provider.id message.order.fulfillment message.order.quote timestamp) items
    let providerInfo =
          DOnSelect.ProviderInfo
            { providerId = providerId,
              name = "",
              url = providerUrl,
              mobileNumber = "", ------------TODO remove it or make it as maybe type
              ridesCompleted = 0 -------------TODO remove it or make it as maybe type
            }
    pure
      DOnSelect.DOnSelectReq
        { bppEstimateId = Id context.message_id,
          ..
        }

buildOnSelectReqV2 ::
  HasFlowEnv m r '["_version" ::: Text] =>
  OnSelect.OnSelectReqV2 ->
  m (Maybe DOnSelect.DOnSelectReq)
buildOnSelectReqV2 req = do
  logDebug $ "on_select requestV2: " <> show req
  let context = req.onSelectReqContext
  timestamp <- context.contextTimestamp & fromMaybeM (InvalidRequest "Missing Timestamp")
  ContextV2.validateContext Context.ON_SELECT context
  handleErrorV2 req $ \message -> do
    providerId <- context.contextBppId & fromMaybeM (InvalidRequest "Missing bpp_id")
    mbProviderUrl <- Utils.getContextBppUri context
    providerUrl <- mbProviderUrl & fromMaybeM (InvalidRequest "Missing bpp_uri")
    order <- message.onSelectReqMessageOrder & fromMaybeM (InvalidRequest "Missing order")
    items <- order.orderItems & fromMaybeM (InvalidRequest "Missing orderItems")
    provider <- order.orderProvider & fromMaybeM (InvalidRequest "Missing orderProvider")
    driverId <- provider.providerId & fromMaybeM (InvalidRequest "Missing providerId")
    fulfillments <- order.orderFulfillments & fromMaybeM (InvalidRequest "Missing orderFulfillments")
    fulfillment <- safeHead fulfillments & fromMaybeM (InvalidRequest "Missing fulfillment")
    quote <- order.orderQuote & fromMaybeM (InvalidRequest "Missing orderQuote")
    quotesInfo <- traverse (buildQuoteInfoV2 driverId fulfillment quote timestamp order) items
    messageUuid <- context.contextMessageId & fromMaybeM (InvalidRequest "Missing message_id")
    let bppEstimateId = Id $ T.pack $ show messageUuid
    let providerInfo =
          DOnSelect.ProviderInfo
            { providerId = providerId,
              name = "",
              url = providerUrl,
              mobileNumber = "", ------------TODO remove it or make it as maybe type
              ridesCompleted = 0 -------------TODO remove it or make it as maybe type
            }
    return $
      Just $
        DOnSelect.DOnSelectReq {..}

handleError ::
  (MonadFlow m) =>
  Either Error OnSelect.OnSelectMessage ->
  (OnSelect.OnSelectMessage -> m DOnSelect.DOnSelectReq) ->
  m (Maybe DOnSelect.DOnSelectReq)
handleError etr action =
  case etr of
    Right msg -> do
      Just <$> action msg
    Left err -> do
      logTagError "on_select req" $ "on_select error: " <> show err
      pure Nothing

handleErrorV2 ::
  (MonadFlow m) =>
  Spec.OnSelectReq ->
  (Spec.OnSelectReqMessage -> m (Maybe DOnSelect.DOnSelectReq)) ->
  m (Maybe DOnSelect.DOnSelectReq)
handleErrorV2 req action =
  case req.onSelectReqError of
    Nothing -> req.onSelectReqMessage & maybe (pure Nothing) action
    Just err -> do
      logTagError "on_select req" $ "on_select error: " <> show err
      pure Nothing

buildQuoteInfo ::
  (MonadThrow m, Log m, MonadTime m) =>
  Text ->
  OnSelect.FulfillmentInfo ->
  OnSelect.Quote ->
  UTCTimeRFC3339 ->
  OnSelect.Item ->
  m DOnSelect.QuoteInfo
buildQuoteInfo driverId fulfillment quote contextTime item = do
  quoteDetails <- case fulfillment._type of
    OnSelect.RIDE -> buildDriverOfferQuoteDetails item fulfillment quote contextTime driverId
    OnSelect.RIDE_OTP -> throwError $ InvalidRequest "select not supported for ride otp trip"
  let vehicleVariant = fulfillment.vehicle.category
      estimatedFare = roundToIntegral item.price.value
      estimatedTotalFare = roundToIntegral item.price.value
      descriptions = []
      specialLocationTag = getTag "general_info" "special_location_tag" =<< item.tags
  validatePrices estimatedFare estimatedTotalFare
  -- if we get here, the discount >= 0, estimatedFare >= estimatedTotalFare
  let discount = if estimatedTotalFare == estimatedFare then Nothing else Just $ estimatedFare - estimatedTotalFare
  pure
    DOnSelect.QuoteInfo
      { vehicleVariant = castVehicleVariant vehicleVariant,
        ..
      }
  where
    castVehicleVariant = \case
      OnSelect.SEDAN -> SEDAN
      OnSelect.SUV -> SUV
      OnSelect.HATCHBACK -> HATCHBACK
      OnSelect.AUTO_RICKSHAW -> AUTO_RICKSHAW
      OnSelect.TAXI -> TAXI
      OnSelect.TAXI_PLUS -> TAXI_PLUS

buildQuoteInfoV2 ::
  (MonadFlow m, MonadThrow m, Log m, MonadTime m) =>
  Text ->
  Spec.Fulfillment ->
  Spec.Quotation ->
  UTCTime ->
  Spec.Order ->
  Spec.Item ->
  m DOnSelect.QuoteInfo
buildQuoteInfoV2 driverId fulfillment quote contextTime order item = do
  fulfillmentType <- fulfillment.fulfillmentType & fromMaybeM (InvalidRequest "Missing fulfillmentType")
  quoteDetails <- case fulfillmentType of
    "RIDE" -> buildDriverOfferQuoteDetailsV2 item fulfillment quote contextTime driverId
    "RIDE_OTP" -> throwError $ InvalidRequest "select not supported for ride otp trip"
    _ -> throwError $ InvalidRequest "Invalid fulfillmentType"
  vehicle <- fulfillment.fulfillmentVehicle & fromMaybeM (InvalidRequest "Missing fulfillmentVehicle")
  vehicleCategory <- vehicle.vehicleCategory & fromMaybeM (InvalidRequest "Missing vehicleCategory")
  vehicleVariant <- castVehicleVariant vehicleCategory & fromMaybeM (InvalidRequest "Invalid vehicleVariant")
  let descriptions = []
      specialLocationTag = getTagV2 "general_info" "special_location_tag" =<< item.itemTags
  case parsedData order of
    Left err -> do
      logTagError "on_select req" $ "on_select error: " <> show err
      throwError $ InvalidRequest "Invalid Price"
    Right (estimatedFare, estimatedTotalFare) -> do
      validatePrices estimatedFare estimatedTotalFare
      -- if we get here, the discount >= 0, estimatedFare >= estimatedTotalFare
      let discount = if estimatedTotalFare == estimatedFare then Nothing else Just $ estimatedFare - estimatedTotalFare
      return $
        DOnSelect.QuoteInfo
          { vehicleVariant = vehicleVariant,
            estimatedFare = Money estimatedFare,
            estimatedTotalFare = Money estimatedTotalFare,
            discount = Money <$> discount,
            ..
          }
  where
    castVehicleVariant = \case
      "SEDAN" -> Just SEDAN
      "SUV" -> Just SUV
      "HATCHBACK" -> Just HATCHBACK
      "AUTO_RICKSHAW" -> Just AUTO_RICKSHAW
      "TAXI" -> Just TAXI
      "TAXI_PLUS" -> Just TAXI_PLUS
      _ -> Nothing

    parsedData :: Spec.Order -> Either Text (Int, Int)
    parsedData orderV2 = do
      estimatedFare <-
        orderV2.orderQuote
          >>= (.quotationPrice)
          >>= (.priceValue)
          >>= parseInt
          & maybe (Left "Invalid Price") Right

      estimatedTotalFare <-
        orderV2.orderQuote
          >>= (.quotationPrice)
          >>= (.priceOfferedValue)
          >>= parseInt
          & maybe (Left "Invalid Offered Price") Right

      Right (estimatedFare, estimatedTotalFare)

    parseInt :: Text -> Maybe Int
    parseInt = readMaybe . T.unpack

buildDriverOfferQuoteDetails ::
  (MonadThrow m, Log m, MonadTime m) =>
  OnSelect.Item ->
  OnSelect.FulfillmentInfo ->
  OnSelect.Quote ->
  UTCTimeRFC3339 ->
  Text ->
  m DOnSelect.DriverOfferQuoteDetails
buildDriverOfferQuoteDetails item fulfillment quote timestamp driverId = do
  driverName <- fulfillment.agent.name & fromMaybeM (InvalidRequest "Missing driver_name in driver offer select item")
  durationToPickup <- getDurationToPickup fulfillment.agent.tags & fromMaybeM (InvalidRequest "Missing duration_to_pickup in driver offer select item")
  distanceToPickup' <- (getDistanceToNearestDriver =<< item.tags) & fromMaybeM (InvalidRequest "Trip type is DRIVER_OFFER, but distance_to_nearest_driver is Nothing")
  validTill <- (getQuoteValidTill (convertRFC3339ToUTC timestamp) =<< quote.ttl) & fromMaybeM (InvalidRequest "Missing valid_till in driver offer select item")
  logDebug $ "on_select ttl request rider: " <> show validTill
  let rating = getDriverRating fulfillment.agent.tags
  bppQuoteId <- (getTag "general_info" "bpp_quote_id" =<< item.tags) & fromMaybeM (InvalidRequest "Missing bpp quoteId select item")
  pure $
    DOnSelect.DriverOfferQuoteDetails
      { distanceToPickup = realToFrac distanceToPickup',
        bppDriverQuoteId = bppQuoteId,
        ..
      }

buildDriverOfferQuoteDetailsV2 ::
  (MonadFlow m, MonadThrow m, Log m, MonadTime m) =>
  Spec.Item ->
  Spec.Fulfillment ->
  Spec.Quotation ->
  UTCTime ->
  Text ->
  m DOnSelect.DriverOfferQuoteDetails
buildDriverOfferQuoteDetailsV2 item fulfillment quote timestamp driverId = do
  fulfillmentTags <- fulfillment.fulfillmentTags & fromMaybeM (InvalidRequest "Missing fulfillmentTags")
  itemTags <- item.itemTags & fromMaybeM (InvalidRequest "Missing itemTags")
  driverName <- tfDriverName fulfillment
  durationToPickup <- getPickupDurationV2 fulfillmentTags & fromMaybeM (InvalidRequest "Missing duration_to_pickup in driver offer select item")
  distanceToPickup' <- getDistanceToNearestDriverV2 itemTags & fromMaybeM (InvalidRequest "Trip type is DRIVER_OFFER, but distance_to_nearest_driver is Nothing")
  validTill <- (getQuoteValidTill timestamp =<< quote.quotationTtl) & fromMaybeM (InvalidRequest "Missing valid_till in driver offer select item")
  let rating = getDriverRatingV2 fulfillmentTags
  bppQuoteId <- (getTagV2 "general_info" "bpp_quote_id" =<< item.itemTags) & fromMaybeM (InvalidRequest "Missing bpp quoteId select item")
  pure $
    DOnSelect.DriverOfferQuoteDetails
      { distanceToPickup = realToFrac distanceToPickup',
        bppDriverQuoteId = bppQuoteId,
        ..
      }

tfDriverName :: MonadFlow m => Spec.Fulfillment -> m Text
tfDriverName fulfillment = do
  agent <- fulfillment.fulfillmentAgent & fromMaybeM (InvalidRequest "Missing fulfillmentAgent")
  agentPerson <- agent.agentPerson & fromMaybeM (InvalidRequest "Missing agentPerson")
  agentPerson.personName & fromMaybeM (InvalidRequest "Missing personName")

getDriverRating :: OnSelect.TagGroups -> Maybe Centesimal
getDriverRating tagGroups = do
  tagValue <- getTag "agent_info" "rating" tagGroups
  driverRating <- readMaybe $ T.unpack tagValue
  Just $ Centesimal driverRating

getDriverRatingV2 :: [Spec.TagGroup] -> Maybe Centesimal
getDriverRatingV2 tagGroups = do
  tagValue <- getTagV2 "agent_info" "rating" tagGroups
  driverRating <- readMaybe $ T.unpack tagValue
  Just $ Centesimal driverRating

getQuoteValidTill :: UTCTime -> Text -> Maybe UTCTime
getQuoteValidTill contextTime time = do
  valid <- parseISO8601Duration time
  Just $ addDurationToUTCTime contextTime valid

getDurationToPickup :: OnSelect.TagGroups -> Maybe Int
getDurationToPickup tagGroups = do
  tagValue <- getTag "agent_info" "duration_to_pickup_in_s" tagGroups
  readMaybe $ T.unpack tagValue

getPickupDurationV2 :: [Spec.TagGroup] -> Maybe Int
getPickupDurationV2 tagGroups = do
  tagValue <- getTagV2 "agent_info" "duration_to_pickup_in_s" tagGroups
  readMaybe $ T.unpack tagValue

getDistanceToNearestDriver :: OnSelect.TagGroups -> Maybe Meters
getDistanceToNearestDriver tagGroups = do
  tagValue <- getTag "general_info" "distance_to_nearest_driver_in_m" tagGroups
  distanceToPickup <- readMaybe $ T.unpack tagValue
  Just $ Meters distanceToPickup

getDistanceToNearestDriverV2 :: [Spec.TagGroup] -> Maybe Meters
getDistanceToNearestDriverV2 tagGroups = do
  tagValue <- getTagV2 "general_info" "distance_to_nearest_driver_in_m" tagGroups
  distanceToPickup <- readMaybe $ T.unpack tagValue
  Just $ Meters distanceToPickup

-- Parse ISO8601 duration and return the number of seconds
parseISO8601Duration :: Text -> Maybe NominalDiffTime
parseISO8601Duration durationStr = do
  (calenderDiffernceTime :: CalendarDiffTime) <- iso8601ParseM $ T.unpack durationStr
  Just $ ctTime calenderDiffernceTime

-- Add the parsed duration to a given UTCTime
addDurationToUTCTime :: UTCTime -> NominalDiffTime -> UTCTime
addDurationToUTCTime time duration = addUTCTime duration time
