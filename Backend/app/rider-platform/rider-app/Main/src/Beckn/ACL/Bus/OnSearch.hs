{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Bus.OnSearch where

import Beckn.ACL.Common (getTag, validatePrices)
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.OnSearch as OnSearch
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import Domain.Types.OnSearchEvent
import qualified Domain.Types.VehicleVariant as VehVar
import EulerHS.Prelude hiding (find, id, map, state, unpack)
import Kernel.Prelude
import Kernel.Product.Validation.Context (validateBusContext)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.OnSearchEvent as OnSearchEvent
import Tools.Error

buildOnSearchReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    EsqDBFlow m r
  ) =>
  BecknCallbackReq OnSearch.OnSearchMessage ->
  m (Maybe DOnSearch.DOnSearchReq)
buildOnSearchReq req = do
  validateBusContext Context.ON_SEARCH $ req.context
  logOnSearchEvent req
  case req.contents of
    Right msg -> do
      let catalog = msg.catalog
      Just <$> searchCbService req.context catalog
    Left err -> do
      logTagError "on_search req" $ "on_search error: " <> show err
      pure Nothing

searchCbService :: MonadFlow m => Context.Context -> OnSearch.Catalog -> m DOnSearch.DOnSearchReq
searchCbService context catalog = do
  providerId <- context.bpp_id & fromMaybeM (InvalidRequest "Missing bpp_id")
  providerUrl <- context.bpp_uri & fromMaybeM (InvalidRequest "Missing bpp_uri")

  let (provider :| _) = catalog.bpp_providers
  let items = provider.items
  quotesInfo <- traverse (buildQuoteInfo provider) items
  let providerInfo =
        DOnSearch.ProviderInfo
          { providerId = providerId,
            name = provider.descriptor.name,
            url = providerUrl,
            mobileNumber = "", ----------TODO----------FIXME : Add Customer support number
            ridesCompleted = 0
          }
  let paymentMethodsInfo = []
  txnId <- fromMaybeM (InvalidRequest "Missing transaction_id") context.transaction_id
  pure
    DOnSearch.DOnSearchReq
      { requestId = Id txnId,
        estimatesInfo = [],
        ..
      }

logOnSearchEvent :: EsqDBFlow m r => OnSearch.OnSearchReq -> m ()
logOnSearchEvent (BecknCallbackReq context (leftToMaybe -> mbErr)) = do
  createdAt <- getCurrentTime
  id <- generateGUID
  bppId <- context.bpp_id & fromMaybeM (InvalidRequest "Missing context.bpp_id")
  let messageId = context.message_id
  let errorType = show.(._type) <$> mbErr
  let errorCode = (.code) <$> mbErr
  let errorMessage = (.message) =<< mbErr
  void $
    OnSearchEvent.create $
      OnSearchEvent {..}

buildQuoteInfo ::
  (MonadThrow m, Log m) =>
  OnSearch.Provider ->
  OnSearch.Item ->
  m DOnSearch.QuoteInfo
buildQuoteInfo provider item = do
  fulfillment <- find (\fulf -> fulf.id == item.fulfillment_id) provider.fulfillments & fromMaybeM (InvalidRequest "Missing fulfillment")
  let itemId = item.id
  let vehicleVariant = castVehicleVariant fulfillment.vehicle.category
      estimatedFare = roundToIntegral item.price.value
      estimatedTotalFare = roundToIntegral item.price.value
      descriptions = []
      specialLocationTag = Nothing
  validatePrices estimatedFare estimatedTotalFare

  -- if we get here, the discount >= 0, estimatedFare >= estimatedTotalFare
  let discount = if estimatedTotalFare == estimatedFare then Nothing else Just $ estimatedFare - estimatedTotalFare
  quoteDetails <- DOnSearch.PublicTransportDetails <$> buildPublicTransportQuoteDetails fulfillment
  pure $ DOnSearch.QuoteInfo {..}
  where
    castVehicleVariant = \case
      OnSearch.SEDAN -> VehVar.SEDAN
      OnSearch.SUV -> VehVar.SUV
      OnSearch.HATCHBACK -> VehVar.HATCHBACK
      OnSearch.AUTO_RICKSHAW -> VehVar.AUTO_RICKSHAW
      OnSearch.TAXI -> VehVar.TAXI
      OnSearch.TAXI_PLUS -> VehVar.TAXI_PLUS
      OnSearch.BUS -> VehVar.BUS

buildPublicTransportQuoteDetails ::
  (MonadThrow m, Log m) =>
  OnSearch.FulfillmentInfo ->
  m DOnSearch.PublicTransportQuoteDetails
buildPublicTransportQuoteDetails fulfillment = do
  tagGroups <- fulfillment.tags & fromMaybeM (InvalidRequest "Missing fulfillment.tags")
  start <- buildStartStop fulfillment tagGroups
  end <- buildEndStop fulfillment tagGroups
  routeInfo <- buildRouteInfo tagGroups
  totalEstimatedDistance <- buildTotalEstimatedDistance tagGroups
  totalDuration <- buildTotalDuration tagGroups
  vehicleServiceType <- buildVehicleServiceType tagGroups
  pure
    DOnSearch.PublicTransportQuoteDetails
      { quoteId = fulfillment.id,
        stops = [],
        ..
      }

buildStartStop ::
  (MonadThrow m, Log m) =>
  OnSearch.FulfillmentInfo ->
  OnSearch.TagGroups ->
  m DOnSearch.Stop
buildStartStop fulfillment tagGroups = do
  let location = fulfillment.start.location
  scheduledTime <- getPickupTime tagGroups
  pure $ DOnSearch.Stop {..}

buildEndStop ::
  (MonadThrow m, Log m) =>
  OnSearch.FulfillmentInfo ->
  OnSearch.TagGroups ->
  m DOnSearch.Stop
buildEndStop fulfillment tagGroups = do
  let location = fulfillment.end.location
  scheduledTime <- getDropOffTime tagGroups
  pure $ DOnSearch.Stop {..}

buildRouteInfo ::
  (MonadThrow m, Log m) =>
  OnSearch.TagGroups ->
  m DOnSearch.RouteInfo
buildRouteInfo tagGroups = do
  routeId <- getRouteId tagGroups
  tripId <- getTripId tagGroups
  routeNo <- getRouteNo tagGroups
  routeName <- getRouteName tagGroups
  pure $ DOnSearch.RouteInfo {..}

getRouteId :: (MonadThrow m, Log m) => OnSearch.TagGroups -> m Text
getRouteId tagGroups = do
  let mbRouteId = getTag routeInfoTagGroupCode routeIdTagCode tagGroups
  mbRouteId & fromMaybeM (InvalidRequest $ buildErrorMessage routeIdTagCode routeInfoTagGroupCode)

getTripId :: (MonadThrow m, Log m) => OnSearch.TagGroups -> m Text
getTripId tagGroups = do
  let mbTripId = getTag routeInfoTagGroupCode tripIdTagCode tagGroups
  mbTripId & fromMaybeM (InvalidRequest $ buildErrorMessage tripIdTagCode routeInfoTagGroupCode)

getRouteNo :: (MonadThrow m, Log m) => OnSearch.TagGroups -> m Text
getRouteNo tagGroups = do
  let mbRouteNo = getTag routeInfoTagGroupCode routeNoTagCode tagGroups
  mbRouteNo & fromMaybeM (InvalidRequest $ buildErrorMessage routeNoTagCode routeInfoTagGroupCode)

getRouteName :: (MonadThrow m, Log m) => OnSearch.TagGroups -> m Text
getRouteName tagGroups = do
  let mbRouteName = getTag routeInfoTagGroupCode routeNameTagCode tagGroups
  mbRouteName & fromMaybeM (InvalidRequest $ buildErrorMessage routeNameTagCode routeInfoTagGroupCode)

getPickupTime :: (MonadThrow m, Log m) => OnSearch.TagGroups -> m Text
getPickupTime tagGroups = do
  let mbPickupTime = getTag routeInfoTagGroupCode pickupTimeTagCode tagGroups
  mbPickupTime & fromMaybeM (InvalidRequest $ buildErrorMessage pickupTimeTagCode routeInfoTagGroupCode)

getDropOffTime :: (MonadThrow m, Log m) => OnSearch.TagGroups -> m Text
getDropOffTime tagGroups = do
  let mbDropOffTime = getTag routeInfoTagGroupCode dropOffTimeTagCode tagGroups
  mbDropOffTime & fromMaybeM (InvalidRequest $ buildErrorMessage dropOffTimeTagCode routeInfoTagGroupCode)

buildTotalEstimatedDistance ::
  (MonadThrow m, Log m) =>
  OnSearch.TagGroups ->
  m Text
buildTotalEstimatedDistance tagGroups = do
  let mbTotalDistance = getTag routeInfoTagGroupCode totalDistanceTagCode tagGroups
  mbTotalDistance & fromMaybeM (InvalidRequest $ buildErrorMessage totalDistanceTagCode routeInfoTagGroupCode)

buildTotalDuration ::
  (MonadThrow m, Log m) =>
  OnSearch.TagGroups ->
  m Text
buildTotalDuration tagGroups = do
  let mbTotalDuration = getTag routeInfoTagGroupCode totalDurationTagCode tagGroups
  mbTotalDuration & fromMaybeM (InvalidRequest $ buildErrorMessage totalDurationTagCode routeInfoTagGroupCode)

buildVehicleServiceType ::
  (MonadThrow m, Log m) =>
  OnSearch.TagGroups ->
  m Text
buildVehicleServiceType tagGroups = do
  let mbVehicleServiceType = getTag vehicleInfoTagGroupCode vehicleServiceTypeTagCode tagGroups
  mbVehicleServiceType & fromMaybeM (InvalidRequest $ buildErrorMessage vehicleServiceTypeTagCode vehicleInfoTagGroupCode)

buildErrorMessage :: TagCode -> TagGroupCode -> Text
buildErrorMessage tagCode tagGroupCode =
  "Missing \'" <> tagCode <> "\' Tag in \'" <> tagGroupCode <> "\' TagGroup in fulfillment.tags"

type TagGroupCode = Text

type TagCode = Text

routeInfoTagGroupCode :: TagGroupCode
routeInfoTagGroupCode = "Route Info."

routeIdTagCode :: TagCode
routeIdTagCode = "Route ID"

tripIdTagCode :: TagCode
tripIdTagCode = "Trip ID"

routeNoTagCode :: TagCode
routeNoTagCode = "Route No"

routeNameTagCode :: TagCode
routeNameTagCode = "Route Name"

totalDistanceTagCode :: TagCode
totalDistanceTagCode = "Total Distance"

totalDurationTagCode :: TagCode
totalDurationTagCode = "Total Duration"

pickupTimeTagCode :: TagCode
pickupTimeTagCode = "From station schedule start time"

dropOffTimeTagCode :: TagCode
dropOffTimeTagCode = "To station schedule start time"

vehicleInfoTagGroupCode :: TagGroupCode
vehicleInfoTagGroupCode = "Vehicle Info"

vehicleServiceTypeTagCode :: TagCode
vehicleServiceTypeTagCode = "Vehicle Service Type"
