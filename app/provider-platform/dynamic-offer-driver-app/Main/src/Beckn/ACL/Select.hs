module Beckn.ACL.Select (buildSelectReq) where

import qualified Beckn.Types.Core.Taxi.API.Select as Select
import qualified Beckn.Types.Core.Taxi.Common.ItemCode as OS
import qualified Domain.Action.Beckn.Select as DSelect
import Domain.Types.Vehicle.Variant
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude hiding (error, setField)
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Tools.Error
import Tools.Metrics (CoreMetrics)

buildSelectReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    CoreMetrics m
  ) =>
  Subscriber.Subscriber ->
  Select.SelectReq ->
  m DSelect.DSelectReq
buildSelectReq subscriber req = do
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
  transactionId <- context.transaction_id & fromMaybeM (InvalidRequest "Missing transaction_id")
  item <- case order.items of
    [item] -> pure item
    _ -> throwError $ InvalidRequest "There should be only one item"
  pure
    DSelect.DSelectReq
      { messageId = messageId,
        transactionId = transactionId,
        bapId = subscriber.subscriber_id,
        bapUri = subscriber.subscriber_url,
        pickupLocation = Maps.LatLong pickup.location.gps.lat pickup.location.gps.lon,
        pickupTime = pickup.time.timestamp,
        dropLocation = Maps.LatLong dropOff.location.gps.lat dropOff.location.gps.lon,
        variant = castVariant item.descriptor.code.vehicleVariant
      }

castVariant :: OS.VehicleVariant -> Variant
castVariant OS.AUTO_RICKSHAW = AUTO_RICKSHAW
castVariant OS.SEDAN = SEDAN
castVariant OS.HATCHBACK = HATCHBACK
castVariant OS.SUV = SUV
