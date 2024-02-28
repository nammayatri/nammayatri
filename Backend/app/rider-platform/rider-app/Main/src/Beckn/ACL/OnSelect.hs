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
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnSelect as OnSelect
import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import BecknV2.Utils
import qualified BecknV2.Utils as Utils
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Domain.Action.Beckn.OnSelect as DOnSelect
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.TimeRFC339 (convertRFC3339ToUTC)
import Kernel.Utils.Common
import Tools.Error

buildOnSelectReqV2 ::
  HasFlowEnv m r '["_version" ::: Text] =>
  OnSelect.OnSelectReqV2 ->
  m (Maybe DOnSelect.DOnSelectReq)
buildOnSelectReqV2 req = do
  logDebug $ "on_select requestV2: " <> show req
  let context = req.onSelectReqContext
  timestamp <- context.contextTimestamp >>= Just . convertRFC3339ToUTC & fromMaybeM (InvalidRequest "Missing Timestamp")
  ContextV2.validateContext Context.ON_SELECT context
  handleErrorV2 req $ \message -> do
    providerId <- context.contextBppId & fromMaybeM (InvalidRequest "Missing bpp_id")
    mbProviderUrl <- Utils.getContextBppUri context
    providerUrl <- mbProviderUrl & fromMaybeM (InvalidRequest "Missing bpp_uri")
    order <- message.onSelectReqMessageOrder & fromMaybeM (InvalidRequest "Missing order")
    items <- order.orderItems & fromMaybeM (InvalidRequest "Missing orderItems")
    fulfillments <- order.orderFulfillments & fromMaybeM (InvalidRequest "Missing orderFulfillments")
    fulfillment <- listToMaybe fulfillments & fromMaybeM (InvalidRequest "Missing fulfillment")
    quote <- order.orderQuote & fromMaybeM (InvalidRequest "Missing orderQuote")
    quotesInfo <- traverse (buildQuoteInfoV2 fulfillment quote timestamp order) items
    messageUuid <- context.contextMessageId & fromMaybeM (InvalidRequest "Missing message_id")
    let bppEstimateId = Id $ UUID.toText messageUuid
        providerInfo =
          DOnSelect.ProviderInfo
            { providerId = providerId,
              name = Nothing,
              url = providerUrl,
              mobileNumber = Nothing -- TODO: get support mobile number from bpp
            }
    pure . Just $
      DOnSelect.DOnSelectReq {..}

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

buildQuoteInfoV2 ::
  (MonadFlow m, MonadThrow m, Log m, MonadTime m) =>
  Spec.Fulfillment ->
  Spec.Quotation ->
  UTCTime ->
  Spec.Order ->
  Spec.Item ->
  m DOnSelect.QuoteInfo
buildQuoteInfoV2 fulfillment quote contextTime order item = do
  fulfillmentType <- fulfillment.fulfillmentType & fromMaybeM (InvalidRequest "Missing fulfillmentType")
  quoteDetails <- case fulfillmentType of
    "DELIVERY" -> buildDriverOfferQuoteDetailsV2 item fulfillment quote contextTime
    "RIDE_OTP" -> throwError $ InvalidRequest "select not supported for ride otp trip"
    _ -> throwError $ InvalidRequest "Invalid fulfillmentType"
  vehicle <- fulfillment.fulfillmentVehicle & fromMaybeM (InvalidRequest "Missing fulfillmentVehicle")
  let mbVariant = Utils.parseVehicleVariant vehicle.vehicleCategory vehicle.vehicleVariant
  vehicleVariant <- mbVariant & fromMaybeM (InvalidRequest $ "Unable to parse vehicleCategory:-" <> show vehicle.vehicleCategory <> ",vehicleVariant:-" <> show vehicle.vehicleVariant)
  let specialLocationTag = Utils.getTagV2 Tag.GENERAL_INFO Tag.SPECIAL_LOCATION_TAG =<< (Just item.itemTags)
  case parsedData order of
    Left err -> do
      logTagError "on_select req" $ "on_select error: " <> show err
      throwError $ InvalidRequest "Invalid or missing price data"
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

buildDriverOfferQuoteDetailsV2 ::
  (MonadFlow m, MonadThrow m, Log m, MonadTime m) =>
  Spec.Item ->
  Spec.Fulfillment ->
  Spec.Quotation ->
  UTCTime ->
  m DOnSelect.DriverOfferQuoteDetails
buildDriverOfferQuoteDetailsV2 item fulfillment quote timestamp = do
  let agentTags = fulfillment.fulfillmentAgent >>= (.agentPerson) >>= (.personTags)
      itemTags = item.itemTags
      driverName = fulfillment.fulfillmentAgent >>= (.agentPerson) >>= (.personName) & fromMaybe "Driver"
      durationToPickup = getPickupDurationV2 agentTags
      distanceToPickup' = getDistanceToNearestDriverV2 itemTags
      rating = getDriverRatingV2 agentTags
  validTill <- (getQuoteValidTill timestamp =<< quote.quotationTtl) & fromMaybeM (InvalidRequest "Missing valid_till in driver offer select item")
  logDebug $ "on_select ttl request rider: " <> show validTill
  bppQuoteId <- fulfillment.fulfillmentId & fromMaybeM (InvalidRequest $ "Missing fulfillmentId, fulfillment:-" <> show fulfillment)
  pure $
    DOnSelect.DriverOfferQuoteDetails
      { distanceToPickup = realToFrac <$> distanceToPickup',
        bppDriverQuoteId = bppQuoteId,
        ..
      }

getDriverRatingV2 :: Maybe [Spec.TagGroup] -> Maybe Centesimal
getDriverRatingV2 tagGroups = do
  tagValue <- Utils.getTagV2 Tag.AGENT_INFO Tag.RATING tagGroups
  driverRating <- readMaybe $ T.unpack tagValue
  Just $ Centesimal driverRating

getQuoteValidTill :: UTCTime -> Text -> Maybe UTCTime
getQuoteValidTill contextTime time = do
  valid <- parseISO8601Duration time
  Just $ addDurationToUTCTime contextTime valid

getPickupDurationV2 :: Maybe [Spec.TagGroup] -> Maybe Int
getPickupDurationV2 tagGroups = do
  tagValue <- Utils.getTagV2 Tag.GENERAL_INFO Tag.ETA_TO_NEAREST_DRIVER_MIN tagGroups
  readMaybe $ T.unpack tagValue

getDistanceToNearestDriverV2 :: Maybe [Spec.TagGroup] -> Maybe Meters
getDistanceToNearestDriverV2 tagGroups = do
  tagValue <- Utils.getTagV2 Tag.GENERAL_INFO Tag.DISTANCE_TO_NEAREST_DRIVER_METER tagGroups
  distanceToPickup <- readMaybe $ T.unpack tagValue
  Just $ Meters distanceToPickup
