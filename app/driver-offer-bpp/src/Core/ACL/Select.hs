module Core.ACL.Select (buildSelectReq) where

import qualified Beckn.External.Maps.Google as GoogleMaps
import Beckn.Prelude hiding (error, setField)
import Beckn.Product.Validation.Context
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Select as Select
import qualified Beckn.Types.Core.Taxi.Common.ItemCode as OS
import qualified Beckn.Types.Registry.Subscriber as Subscriber
import Beckn.Utils.Common
import qualified Domain.Action.Beckn.Select as DSelect
import Domain.Types.SearchRequest.SearchReqLocation (SearchReqLocationAPIEntity (..))
import Domain.Types.Vehicle.Variant
import SharedLogic.GoogleMaps (Address (..), mkLocation)
import Tools.Error
import Tools.Metrics (CoreMetrics)

buildSelectReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    GoogleMaps.HasGoogleCfg r,
    CoreMetrics m
  ) =>
  Text ->
  Subscriber.Subscriber ->
  Select.SelectReq ->
  m DSelect.DSelectReq
buildSelectReq sessiontoken subscriber req = do
  let context = req.context
  validateContext Context.SELECT context
  let order = req.message.order
  let pickup = order.fulfillment.start
  let dropOff = fromJust order.fulfillment.end
  unless (subscriber.subscriber_id == context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == context.bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")
  let messageId = context.message_id
  item <- case order.items of
    [item] -> pure item
    _ -> throwError $ InvalidRequest "There should be only one item"
  pickupRes <- GoogleMaps.getPlaceName (Just sessiontoken) (GoogleMaps.ByLatLong $ GoogleMaps.LatLong pickup.location.gps.lat pickup.location.gps.lon) Nothing
  pickUpAddress <- mkLocation pickupRes
  pickupLocation <- buildSearchReqLocationAPIEntity pickUpAddress (pickup.location.gps.lat) (pickup.location.gps.lon)
  dropOffRes <- GoogleMaps.getPlaceName (Just sessiontoken) (GoogleMaps.ByLatLong $ GoogleMaps.LatLong dropOff.location.gps.lat dropOff.location.gps.lon) Nothing
  dropOffAddress <- mkLocation dropOffRes
  dropLocation <- buildSearchReqLocationAPIEntity dropOffAddress (dropOff.location.gps.lat) (dropOff.location.gps.lon)
  pure
    DSelect.DSelectReq
      { messageId = messageId,
        transactionId = context.transaction_id,
        bapId = subscriber.subscriber_id,
        bapUri = subscriber.subscriber_url,
        pickupLocation,
        pickupTime = pickup.time.timestamp,
        dropLocation,
        variant = castVariant item.descriptor.code.vehicleVariant
      }

buildSearchReqLocationAPIEntity :: (MonadFlow m, CoreMetrics m) => Address -> Double -> Double -> m SearchReqLocationAPIEntity
buildSearchReqLocationAPIEntity Address {..} lat lon = pure SearchReqLocationAPIEntity {..}

castVariant :: OS.VehicleVariant -> Variant
castVariant OS.AUTO_RICKSHAW = AUTO_RICKSHAW
castVariant OS.SEDAN = SEDAN
castVariant OS.HATCHBACK = HATCHBACK
castVariant OS.SUV = SUV
