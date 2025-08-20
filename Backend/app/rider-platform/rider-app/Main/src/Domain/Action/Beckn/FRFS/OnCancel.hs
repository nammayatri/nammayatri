{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.FRFS.OnCancel where

import qualified BecknV2.FRFS.Enums as Spec
import qualified Domain.Action.Beckn.FRFS.Common as Common
import qualified Domain.Types.FRFSTicketBooking as Booking
import qualified Domain.Types.FRFSTicketBooking as FTBooking
import qualified Domain.Types.FRFSTicketBookingStatus as FTBooking
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import Domain.Types.Merchant as Merchant
import qualified Domain.Types.Trip as DTrip
import Environment
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Types as JL
import qualified Lib.JourneyModule.Base as JM
import qualified Lib.JourneyModule.State.Types as JMState
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.FRFSRecon as QFRFSRecon
import qualified Storage.Queries.FRFSTicketBooking as QTBooking

data DOnCancel = DOnCancel
  { providerId :: Text,
    totalPrice :: HighPrecMoney,
    bppOrderId :: Text,
    bppItemId :: Text,
    transactionId :: Text,
    messageId :: Text,
    orderStatus :: Spec.OrderStatus,
    refundAmount :: Maybe HighPrecMoney,
    baseFare :: HighPrecMoney,
    cancellationCharges :: Maybe HighPrecMoney
  }

validateRequest :: DOnCancel -> Flow (Merchant, FTBooking.FRFSTicketBooking)
validateRequest DOnCancel {..} = do
  booking <- runInReplica $ QTBooking.findBySearchId (Id transactionId) >>= fromMaybeM (BookingDoesNotExist messageId)
  let merchantId = booking.merchantId
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  when (totalPrice /= baseFare + fromMaybe 0 refundAmount + fromMaybe 0 cancellationCharges) $ throwError (InternalError "Fare Mismatch in onCancel Req")
  return (merchant, booking)

onCancel :: Merchant -> Booking.FRFSTicketBooking -> DOnCancel -> Flow ()
onCancel merchant booking' dOnCancel = do
  let booking = booking' {Booking.bppOrderId = Just dOnCancel.bppOrderId}
  let refundAmount = fromMaybe (negate dOnCancel.baseFare) dOnCancel.refundAmount
  let cancellationCharges = fromMaybe 0 dOnCancel.cancellationCharges
  case dOnCancel.orderStatus of
    Spec.SOFT_CANCELLED -> do
      void $ QTBooking.updateRefundCancellationChargesAndIsCancellableByBookingId (Just refundAmount) (Just cancellationCharges) (Just True) booking.id
    Spec.CANCELLED -> Common.handleCancelledStatus merchant booking refundAmount cancellationCharges dOnCancel.messageId True
    Spec.CANCEL_INITIATED -> do
      void $ QTBooking.updateStatusById FTBooking.CANCEL_INITIATED booking.id
      void $ QFRFSRecon.updateStatusByTicketBookingId (Just DFRFSTicket.CANCEL_INITIATED) booking.id
      whenJust booking.journeyId $ \journeyId -> do
        allLegsInfo <- JM.getAllLegsInfo journeyId False
        forM_ allLegsInfo $ \journeyLegInfo -> do
          case journeyLegInfo.travelMode of
            DTrip.Walk -> JM.markLegStatus (Just JL.Cancelled) (Just JMState.Finished) journeyLegInfo journeyId Nothing
            DTrip.Taxi -> JM.markLegStatus (Just JL.Cancelled) (Just JMState.Finished) journeyLegInfo journeyId Nothing
            _ -> do
              unless (null booking'.journeyRouteDetails) $ do
                forM_ booking'.journeyRouteDetails $ \routeDetails -> do
                  JM.markLegStatus (Just JL.Cancelled) (Just JMState.Finished) journeyLegInfo journeyId routeDetails.subLegOrder
        journey <- JM.getJourney journeyId
        updatedLegStatus <- JM.getAllLegsStatus journey
        JM.checkAndMarkTerminalJourneyStatus journey False False updatedLegStatus
    _ -> throwError $ InvalidRequest "Unexpected orderStatus received"
