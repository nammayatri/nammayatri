{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
  let customerExtraFee = listToMaybe order.quote.breakup <&> roundToIntegral . (.price.value)
  pure
    DSelect.DSelectReq
      { messageId = messageId,
        transactionId = transactionId,
        bapId = subscriber.subscriber_id,
        bapUri = subscriber.subscriber_url,
        pickupLocation = Maps.LatLong pickup.location.gps.lat pickup.location.gps.lon,
        pickupTime = pickup.time.timestamp,
        dropLocation = Maps.LatLong dropOff.location.gps.lat dropOff.location.gps.lon,
        pickupAddress = pickup.location.address,
        dropAddrress = dropOff.location.address,
        variant = castVariant item.descriptor.code.vehicleVariant,
        autoAssignEnabled = order.fulfillment.tags.auto_assign_enabled,
        customerExtraFee = customerExtraFee
      }

castVariant :: OS.VehicleVariant -> Variant
castVariant OS.AUTO_RICKSHAW = AUTO_RICKSHAW
castVariant OS.SEDAN = SEDAN
castVariant OS.HATCHBACK = HATCHBACK
castVariant OS.SUV = SUV
