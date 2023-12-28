{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Update (buildUpdateReq) where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.Types.Core.Taxi.API.Update as Update
import qualified Beckn.Types.Core.Taxi.Update as Update
import qualified Beckn.Types.Core.Taxi.Update.UpdateEvent.PaymentCompletedEvent as Update
import qualified Domain.Action.Beckn.Update as DUpdate
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import EulerHS.Prelude hiding (state)
import Kernel.Product.Validation.Context (validateContext)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Tools.Error (GenericError (InvalidRequest))

buildUpdateReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text]
  ) =>
  Subscriber.Subscriber ->
  Update.UpdateReq ->
  m DUpdate.DUpdateReq
buildUpdateReq subscriber req = do
  validateContext Context.UPDATE $ req.context
  unless (subscriber.subscriber_id == req.context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == req.context.bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")
  pure $ parseEvent req.message.order

parseEvent :: Update.UpdateEvent -> DUpdate.DUpdateReq
parseEvent (Update.PaymentCompleted pcEvent) = do
  DUpdate.PaymentCompletedReq
    { bookingId = Id pcEvent.id,
      rideId = Id pcEvent.fulfillment.id,
      paymentStatus = castPaymentStatus pcEvent.payment.status,
      paymentMethodInfo = mkPaymentMethodInfo pcEvent.payment
    }
parseEvent (Update.EditLocation elEvent) = do
  DUpdate.EditLocationReq
    { bookingId = Id elEvent.id,
      rideId = Id elEvent.fulfillment.id,
      origin = elEvent.fulfillment.origin.location,
      destination = elEvent.fulfillment.destination.location
    }

mkPaymentMethodInfo :: Update.Payment -> DMPM.PaymentMethodInfo
mkPaymentMethodInfo Update.Payment {..} =
  DMPM.PaymentMethodInfo
    { collectedBy = Common.castPaymentCollector collected_by,
      paymentType = Common.castPaymentType _type,
      paymentInstrument = Common.castPaymentInstrument instrument
    }

castPaymentStatus :: Update.PaymentStatus -> DUpdate.PaymentStatus
castPaymentStatus Update.PAID = DUpdate.PAID
castPaymentStatus Update.NOT_PAID = DUpdate.NOT_PAID
