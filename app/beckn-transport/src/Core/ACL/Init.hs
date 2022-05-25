module Core.ACL.Init where

import Beckn.Prelude
import Beckn.Product.Validation.Context
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Init as Init
import qualified Beckn.Types.Core.Taxi.Init as Init
import Beckn.Types.MapSearch
import qualified Beckn.Types.Registry as Subscriber
import qualified Domain.Action.Beckn.Init as DInit
import qualified Domain.Types.FareProduct as FP
import qualified Domain.Types.Vehicle as Veh
import Types.Error
import Utils.Common

buildInitReq ::
  (HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]) =>
  Subscriber.Subscriber ->
  Init.InitReq ->
  m DInit.InitReq
buildInitReq subscriber req = do
  let context = req.context
      items = req.message.order.items
      fulfillment = req.message.order.fulfillment
  validateContext Context.INIT context
  item <- case items of
    [item] -> return item
    _ -> throwError (InvalidRequest "order.items must contain exactly 1 item.")
  unless (subscriber.subscriber_id == context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  let itemCode = item.descriptor.code
  return $
    DInit.InitReq
      { vehicleVariant = castVehicleVariant itemCode.vehicleVariant,
        fareProductType = castFareProductType itemCode.fareProductType,
        distance = itemCode.distance,
        duration = itemCode.duration,
        fromLocation = LatLong {lat = fulfillment.start.location.gps.lat, lon = fulfillment.start.location.gps.lon},
        toLocation = fulfillment.end <&> \end -> LatLong {lat = end.location.gps.lat, lon = end.location.gps.lon},
        bapId = subscriber.subscriber_id,
        bapUri = subscriber.subscriber_url,
        startTime = fulfillment.start.time.timestamp
      }
  where
    castVehicleVariant = \case
      Init.SUV -> Veh.SUV
      Init.HATCHBACK -> Veh.HATCHBACK
      Init.SEDAN -> Veh.SEDAN
    castFareProductType = \case
      Init.ONE_WAY_TRIP -> FP.ONE_WAY
      Init.RENTAL_TRIP -> FP.RENTAL
