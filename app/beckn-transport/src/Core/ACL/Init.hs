module Core.ACL.Init where

import Beckn.Prelude
import qualified Beckn.Types.Core.Taxi.API.Init as Init
import qualified Beckn.Types.Core.Taxi.Init as Init
import Beckn.Types.Logging
import Beckn.Types.MapSearch
import qualified Beckn.Types.Registry as Subscriber
import qualified Domain.Action.Beckn.Init as DInit
import qualified Domain.Types.FareProduct as FP
import qualified Domain.Types.Vehicle as Veh
import Types.Error
import Utils.Common

buildInitReq ::
  (MonadThrow m, Log m) =>
  Subscriber.Subscriber ->
  Init.InitReq ->
  m DInit.InitReq
buildInitReq subscriber req = do
  let context = req.context
      items = req.message.order.items
      fulfillment = req.message.order.fulfillment
  item <- case items of
    [item] -> return item
    _ -> throwError (InvalidRequest "order.items must contain exactly 1 item.")
  unless (subscriber.subscriber_id == context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  return $
    DInit.InitReq
      { vehicleVariant = castVehicleVariant item.descriptor.code.vehicleVariant,
        fareProductType = castFareProductType item.descriptor.code.fareProductType,
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
      Init.RENTAL_TRIP dist dur -> FP.RENTAL
