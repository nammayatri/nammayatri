{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.FRFS.OnCancel (module Domain.Types.Beckn.FRFS.OnCancel, validateRequest, onCancel) where

import qualified BecknV2.FRFS.Enums as Spec
import Domain.Action.Beckn.FRFS.OnCancel.Core (onCancelCore)
import Domain.Types.Beckn.FRFS.OnCancel
import qualified Domain.Types.FRFSTicketBooking as Booking
import qualified Domain.Types.FRFSTicketBooking as FTBooking
import Domain.Types.Merchant as Merchant
import Environment
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FRFSCancel as FRFSCancel
import qualified SharedLogic.FRFSCancelJourney as FRFSCancelJourney
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.FRFSTicketBooking as QTBooking

validateRequest :: DOnCancel -> Flow (Merchant, FTBooking.FRFSTicketBooking)
validateRequest DOnCancel {..} = do
  booking <- runInReplica $ QTBooking.findBySearchId (Id transactionId) >>= fromMaybeM (BookingDoesNotExist messageId)
  let merchantId = booking.merchantId
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  when (totalPrice /= baseFare + fromMaybe 0 refundAmount + fromMaybe 0 cancellationCharges) $ throwError (InternalError "Fare Mismatch in onCancel Req")
  return (merchant, booking)

onCancel :: Merchant -> Booking.FRFSTicketBooking -> DOnCancel -> Flow ()
onCancel merchant booking dOnCancel = do
  let booking' = booking {Booking.bppOrderId = Just dOnCancel.bppOrderId}
  mbSideEffectData <- onCancelCore merchant booking dOnCancel
  whenJust mbSideEffectData $ \(mRiderNumber, mRiderMobileCountryCode, fareParameters) ->
    FRFSCancel.handleCancelledSideEffects (booking {Booking.bppOrderId = Just dOnCancel.bppOrderId}) mRiderNumber mRiderMobileCountryCode fareParameters
  when (isFinalCancelStatus dOnCancel.orderStatus) $
    FRFSCancelJourney.cancelJourney booking'
  where
    isFinalCancelStatus status =
      status `elem` [Spec.CANCELLED, Spec.CANCEL_INITIATED]
