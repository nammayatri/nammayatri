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
import qualified Beckn.Types.Core.Taxi.API.OnSelect as OnSelect
import qualified Beckn.Types.Core.Taxi.OnSelect as OnSelect
import qualified Data.Text as T
import qualified Domain.Action.Beckn.OnSelect as DOnSelect
import Domain.Types.VehicleVariant
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Error

-- import qualified Beckn.Types.Core.Taxi.Common.FulfillmentType as OnSelect

buildOnSelectReq ::
  HasFlowEnv m r '["coreVersion" ::: Text] =>
  OnSelect.OnSelectReq ->
  m (Maybe DOnSelect.DOnSelectReq)
buildOnSelectReq req = do
  logDebug $ "on_select request: " <> show req
  let context = req.context
  validateContext Context.ON_SELECT context
  handleError req.contents $ \message -> do
    providerId <- context.bpp_id & fromMaybeM (InvalidRequest "Missing bpp_id")
    providerUrl <- context.bpp_uri & fromMaybeM (InvalidRequest "Missing bpp_uri")
    -- let provider = message.order.provider
    let items = message.order.items
    quotesInfo <- traverse (buildQuoteInfo message.order.fulfillment) items
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

buildQuoteInfo ::
  (MonadThrow m, Log m) =>
  OnSelect.FulfillmentInfo ->
  OnSelect.Item ->
  m DOnSelect.QuoteInfo
buildQuoteInfo fulfillment item = do
  quoteDetails <- case fulfillment._type of
    -- OnSelect.ONE_WAY_TRIP -> throwError $ InvalidRequest "select not supported for one way trip"
    -- OnSelect.RENTAL_TRIP -> throwError $ InvalidRequest "select not supported for rental trip"
    OnSelect.RIDE -> buildDriverOfferQuoteDetails item fulfillment
    -- OnSelect.DRIVER_OFFER -> buildDriverOfferQuoteDetails item
    -- OnSelect.DRIVER_OFFER_ESTIMATE -> throwError $ InvalidRequest "Estimates are only supported in on_search"
    OnSelect.RIDE_OTP -> throwError $ InvalidRequest "select not supported for ride otp trip"
  -- let itemCode = item.descriptor.code
  let vehicleVariant = fulfillment.vehicle.category
      estimatedFare = roundToIntegral item.price.value
      estimatedTotalFare = roundToIntegral item.price.offered_value
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

buildDriverOfferQuoteDetails ::
  (MonadThrow m, Log m) =>
  OnSelect.Item ->
  OnSelect.FulfillmentInfo ->
  m DOnSelect.DriverOfferQuoteDetails
buildDriverOfferQuoteDetails item fulfillment = do
  driverName <- fulfillment.agent.name & fromMaybeM (InvalidRequest "Missing driver_name in driver offer select item")
  durationToPickup <- getDurationToPickup fulfillment.agent.tags & fromMaybeM (InvalidRequest "Missing duration_to_pickup in driver offer select item")
  distanceToPickup' <- (getDistanceToNearestDriver =<< item.tags) & fromMaybeM (InvalidRequest "Trip type is DRIVER_OFFER, but distance_to_nearest_driver is Nothing")
  -- (item.tags <&> (.distance_to_nearest_driver))
  --   & fromMaybeM (InvalidRequest "Trip type is DRIVER_OFFER, but distance_to_nearest_driver is Nothing")
  validTill <- item.valid_till & fromMaybeM (InvalidRequest "Missing valid_till in driver offer select item")
  let rating = getDriverRating fulfillment.agent.tags
  bppQuoteId <- (getTag "general_info" "bpp_quote_id" =<< item.tags) & fromMaybeM (InvalidRequest "Missing bpp quoteId select item")
  pure $
    DOnSelect.DriverOfferQuoteDetails
      { distanceToPickup = realToFrac distanceToPickup',
        bppDriverQuoteId = Id bppQuoteId,
        ..
      }

-- getDriverName :: OnSelect.Agent -> Maybe Text
-- getDriverName agent = (.name) =<< fulfillment.agent

getDriverRating :: [OnSelect.TagGroup] -> Maybe Centesimal
getDriverRating tagGroups = do
  -- tagGroup <- find (\tagGroup -> tagGroup.code == "agent_info") tagGroups
  -- tag <- find (\tag -> tag.code == Just "rating") tagGroup.list
  tagValue <- getTag "agent_info" "rating" tagGroups
  driverRating <- readMaybe $ T.unpack tagValue
  Just $ Centesimal driverRating

getDurationToPickup :: [OnSelect.TagGroup] -> Maybe Int
getDurationToPickup tagGroups = do
  -- tagGroup <- find (\tagGroup -> tagGroup.code == "agent_info") tagGroups
  -- tag <- find (\tag -> tag.code == Just "duration_to_pickup_in_s") tagGroup.list
  tagValue <- getTag "agent_info" "duration_to_pickup_in_s" tagGroups
  readMaybe $ T.unpack tagValue

getDistanceToNearestDriver :: [OnSelect.TagGroup] -> Maybe Meters
getDistanceToNearestDriver tagGroups = do
  tagValue <- getTag "general_info" "distance_to_nearest_driver_in_m" tagGroups
  distanceToPickup <- readMaybe $ T.unpack tagValue
  Just $ Meters distanceToPickup

getTag :: Text -> Text -> [OnSelect.TagGroup] -> Maybe Text
getTag tagGroupCode tagCode tagGroups = do
  tagGroup <- find (\tagGroup -> tagGroup.code == tagGroupCode) tagGroups
  tag <- find (\tag -> tag.code == Just tagCode) tagGroup.list
  tag.value

-- getBppQuoteId :: [OnSelect.TagGroup] -> Maybe Text
-- getBppQuoteId tagGroups = do
--   tagGroup <- find (\tagGroup -> tagGroup.code == "general_info") tagGroups
--   tag <- find (\tag -> tag.code == Just "bpp_quote_id") tagGroup.list
--   tag.value
