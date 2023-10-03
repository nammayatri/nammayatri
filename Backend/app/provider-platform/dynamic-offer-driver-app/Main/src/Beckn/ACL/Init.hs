{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Init where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.Types.Core.Taxi.API.Init as Init
import qualified Beckn.Types.Core.Taxi.Init as Init
import qualified Data.Text as T
import qualified Domain.Action.Beckn.Init as DInit
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Vehicle.Variant as VehVar
import Kernel.Prelude
import qualified Kernel.Product.Validation.Context as Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Field
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Error.Throwing

buildInitReq ::
  ( MonadThrow m,
    HasFlowEnv m r '["coreVersion" ::: Text]
  ) =>
  Subscriber.Subscriber ->
  Init.InitReq ->
  m DInit.InitReq
buildInitReq subscriber req = do
  let context = req.context
  Context.validateContext Context.INIT context
  let order = req.message.order
  _ <- case order.items of
    [it] -> pure it
    _ -> throwError $ InvalidRequest "There must be exactly one item in init request"
  fulfillmentId <- order.fulfillment.id & fromMaybeM (InvalidRequest "FulfillmentId not found. It should either be estimateId or quoteId")
  let maxEstimatedDistance = getMaxEstimateDistance =<< order.fulfillment.tags
  let initTypeReq = buildInitTypeReq order.fulfillment._type
  -- should we check start time and other details?
  unless (subscriber.subscriber_id == context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == context.bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")

  pure
    DInit.InitReq
      { estimateId = fulfillmentId,
        bapId = subscriber.subscriber_id,
        bapUri = subscriber.subscriber_url,
        bapCity = context.city,
        bapCountry = context.country,
        vehicleVariant = castVehicleVariant order.fulfillment.vehicle.category,
        driverId = order.provider <&> (.id),
        paymentMethodInfo = mkPaymentMethodInfo order.payment,
        ..
      }
  where
    buildInitTypeReq = \case
      Init.RIDE_OTP -> DInit.InitSpecialZoneReq
      Init.RIDE -> DInit.InitNormalReq
    castVehicleVariant = \case
      Init.SEDAN -> VehVar.SEDAN
      Init.SUV -> VehVar.SUV
      Init.HATCHBACK -> VehVar.HATCHBACK
      Init.AUTO_RICKSHAW -> VehVar.AUTO_RICKSHAW
      Init.TAXI -> VehVar.TAXI
      Init.TAXI_PLUS -> VehVar.TAXI_PLUS
      Init.BUS -> VehVar.BUS

mkPaymentMethodInfo :: Init.Payment -> Maybe DMPM.PaymentMethodInfo
mkPaymentMethodInfo Init.Payment {..} =
  params.instrument <&> \instrument' -> do
    DMPM.PaymentMethodInfo
      { collectedBy = Common.castPaymentCollector params.collected_by,
        paymentType = Common.castPaymentType _type,
        paymentInstrument = Common.castPaymentInstrument instrument'
      }

getMaxEstimateDistance :: Init.TagGroups -> Maybe HighPrecMeters
getMaxEstimateDistance tagGroups = do
  tagValue <- Common.getTag "estimations" "max_estimated_distance" tagGroups
  maxEstimatedDistance <- readMaybe $ T.unpack tagValue
  Just $ HighPrecMeters maxEstimatedDistance
