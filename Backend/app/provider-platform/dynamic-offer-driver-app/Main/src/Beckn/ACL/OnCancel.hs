{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnCancel
  ( buildOnCancelMessageV2,
    module Reexport,
  )
where

import qualified Beckn.OnDemand.Utils.Common as BUtils
import qualified Beckn.OnDemand.Utils.OnCancel as Utils
import qualified Beckn.Types.Core.Taxi.OnCancel.OnCancelEvent.OnCancelEventType as Event
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as CU
import qualified Data.List as List
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Merchant as DM
import Domain.Types.OnCancel as Reexport
import qualified Domain.Types.OnCancel as OU
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common

buildOnCancelMessageV2 ::
  ( MonadFlow m,
    EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  DM.Merchant ->
  Maybe Context.City ->
  Maybe Context.Country ->
  Text ->
  OnCancelBuildReq ->
  m Spec.OnCancelReq
buildOnCancelMessageV2 merchant mbBapCity mbBapCountry cancelStatus req = do
  msgId <- generateGUID
  let bppId = getShortId $ merchant.subscriberId
      city = fromMaybe merchant.city mbBapCity
      country = fromMaybe merchant.country mbBapCountry
  bppUri <- BUtils.mkBppUri merchant.id.getId
  buildOnCancelReqV2 Context.ON_CANCEL Context.MOBILITY msgId bppId bppUri city country cancelStatus req

buildOnCancelReqV2 ::
  (MonadFlow m, EncFlow m r) =>
  Context.Action ->
  Context.Domain ->
  Text ->
  Text ->
  BaseUrl ->
  Context.City ->
  Context.Country ->
  Text ->
  OU.OnCancelBuildReq ->
  m Spec.OnCancelReq
buildOnCancelReqV2 action domain messageId bppSubscriberId bppUri city country cancelStatus = \case
  OU.BookingCancelledBuildReqV2 OU.DBookingCancelledReqV2 {..} -> do
    context <- CU.buildContextV2 action domain messageId (Just booking.transactionId) booking.bapId booking.bapUri (Just bppSubscriberId) (Just bppUri) city country
    pure $
      Spec.OnCancelReq
        { onCancelReqError = Nothing,
          onCancelReqContext = context,
          onCancelReqMessage = buildOnCancelMessageReqV2 booking cancelStatus cancellationSource
        }

buildOnCancelMessageReqV2 :: DRB.Booking -> Text -> SBCR.CancellationSource -> Maybe Spec.ConfirmReqMessage
buildOnCancelMessageReqV2 booking cancelStatus cancellationSource =
  Just $
    Spec.ConfirmReqMessage
      { confirmReqMessageOrder = tfOrder booking cancelStatus cancellationSource
      }

tfOrder :: DRB.Booking -> Text -> SBCR.CancellationSource -> Spec.Order
tfOrder booking cancelStatus cancellationSource =
  Spec.Order
    { orderId = Just booking.id.getId,
      orderStatus = Just cancelStatus,
      orderFulfillments = Just . List.singleton $ tfFulfillments,
      orderCancellation = tfCancellation cancellationSource,
      orderBilling = Nothing,
      orderCancellationTerms = Nothing,
      orderItems = Nothing,
      orderPayments = Nothing,
      orderProvider = Nothing,
      orderQuote = Nothing
    }

tfFulfillments :: Spec.Fulfillment
tfFulfillments =
  Spec.Fulfillment
    { fulfillmentState = mkFulfillmentState,
      fulfillmentId = Nothing,
      fulfillmentStops = Nothing,
      fulfillmentType = Nothing,
      fulfillmentAgent = Nothing,
      fulfillmentCustomer = Nothing,
      fulfillmentTags = Nothing,
      fulfillmentVehicle = Nothing
    }
  where
    mkFulfillmentState =
      Just $
        Spec.FulfillmentState
          { fulfillmentStateDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Event.RIDE_BOOKING_CANCELLED, -- TODO::Beckn, decide on fulfillment.state.descriptor.code mapping according to spec-v2
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  }
          }

tfCancellation :: SBCR.CancellationSource -> Maybe Spec.Cancellation
tfCancellation cancellationSource =
  Just $
    Spec.Cancellation
      { cancellationCancelledBy = Just . show $ Utils.castCancellationSource cancellationSource
      }
