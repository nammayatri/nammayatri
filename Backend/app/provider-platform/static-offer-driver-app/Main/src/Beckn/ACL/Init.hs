{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Init where

import qualified Beckn.Types.Core.Taxi.API.Init as Init
import qualified Beckn.Types.Core.Taxi.Init as Init
import qualified Domain.Action.Beckn.Init as DInit
import qualified Domain.Types.Vehicle as Veh
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Registry as Subscriber
import Kernel.Utils.Common
import Tools.Error

buildInitReq ::
  (HasFlowEnv m r '["coreVersion" ::: Text]) =>
  Subscriber.Subscriber ->
  Init.InitReq ->
  m DInit.InitReq
buildInitReq subscriber req = do
  validateContext Context.INIT context
  item <- case items of
    [item] -> return item
    _ -> throwError (InvalidRequest "order.items must contain exactly 1 item.")
  unless (subscriber.subscriber_id == context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  let itemCode = item.descriptor.code
  initTypeReq <- buildInitTypeReq item
  vehicleVariant <- fromEitherM InvalidRequest $ castVehicleVariant itemCode.vehicleVariant
  transactionId <- context.transaction_id & fromMaybeM (InvalidRequest "Missing transaction_id")
  return $
    DInit.InitReq
      { vehicleVariant, -- = castVehicleVariant itemCode.vehicleVariant,
        transactionId = transactionId,
        fromLocation = LatLong {lat = fulfillment.start.location.gps.lat, lon = fulfillment.start.location.gps.lon},
        -- toLocation = fulfillment.end <&> \end -> LatLong {lat = end.location.gps.lat, lon = end.location.gps.lon},
        bapId = subscriber.subscriber_id,
        bapUri = subscriber.subscriber_url,
        startTime = fulfillment.start.time.timestamp,
        initTypeReq = initTypeReq
      }
  where
    context = req.context
    items = req.message.order.items
    fulfillment = req.message.order.fulfillment

    castVehicleVariant = \case
      Init.SUV -> Right Veh.SUV
      Init.HATCHBACK -> Right Veh.HATCHBACK
      Init.SEDAN -> Right Veh.SEDAN
      Init.AUTO_RICKSHAW -> Left "Auto-rickshaw vehicles are not supported by this BPP"
    buildInitTypeReq item = do
      let itemCode = item.descriptor.code
      case itemCode.fareProductType of
        Init.ONE_WAY_TRIP -> do
          toLocationInfo <- fulfillment.end & fromMaybeM (InternalError "FareProductType is ONE_WAY but ToLocation is Nothing")
          let toLocation = LatLong {lat = toLocationInfo.location.gps.lat, lon = toLocationInfo.location.gps.lon}
          return . DInit.InitOneWayTypeReq $
            DInit.InitOneWayReq
              { ..
              }
        Init.RENTAL_TRIP -> do
          distance <- itemCode.distance & fromMaybeM (InternalError "FareProductType is RENTAL but distance is Nothing")
          duration <- itemCode.duration & fromMaybeM (InternalError "FareProductType is RENTAL but duration is Nothing")
          return . DInit.InitRentalTypeReq $
            DInit.InitRentalReq
              { ..
              }
        Init.DRIVER_OFFER_ESTIMATE -> throwError $ InvalidRequest "Driver offer is not supported by this BPP"
        Init.DRIVER_OFFER -> throwError $ InvalidRequest "Driver offer is not supported by this BPP"
        Init.ONE_WAY_SPECIAL_ZONE -> throwError $ InvalidRequest "one way special zone is not supported by this BPP"
