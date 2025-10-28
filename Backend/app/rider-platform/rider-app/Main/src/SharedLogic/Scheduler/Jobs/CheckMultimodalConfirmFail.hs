{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.CheckMultimodalConfirmFail where

import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBooking
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.FRFSUtils as FRFSUtils
import SharedLogic.JobScheduler
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment

checkMultimodalConfirmFailJob ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    EsqDBReplicaFlow m r,
    ServiceFlow m r
  ) =>
  Job 'CheckMultimodalConfirmFail ->
  m ExecutionResult
checkMultimodalConfirmFailJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      bookingId = jobData.bookingId
  booking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest $ "booking not found for id: " <> show bookingId)
  frfsTickets <- QFRFSTicket.findAllByTicketBookingId bookingId

  bookingPayments <- QFRFSTicketBookingPayment.findAllTBPByBookingId bookingId

  mapM_
    ( \bookingPayment -> do
        when ((booking.status == DFRFSTicketBooking.FAILED || null frfsTickets) && isBookingPaymentInTerminalState bookingPayment) $ do
          FRFSUtils.markAllRefundBookings booking bookingPayment booking.riderId
    )
    bookingPayments

  if all isBookingPaymentInTerminalState bookingPayments
    then return Complete
    else return Complete -- TODO :: We should do some retry in this case till payment booking is made terminal.
  where
    isBookingPaymentInTerminalState bookingPayment = bookingPayment.status `elem` [DFRFSTicketBookingPayment.SUCCESS, DFRFSTicketBookingPayment.REFUND_PENDING]
