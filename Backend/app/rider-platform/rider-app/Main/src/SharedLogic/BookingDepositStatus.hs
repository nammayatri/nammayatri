module SharedLogic.BookingDepositStatus
  ( DepositLedgerView (..),
    depositDisplayStatus,
    withBookingDeposit,
  )
where

import qualified API.Types.UI.BookingDeposit as API
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.API as SRB
import qualified Domain.Types.BookingPayment as DBP
import qualified Domain.Types.BookingStatus as DRB
import Environment (Flow)
import Kernel.Prelude
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Finance.Ledger.Service as Ledger
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified SharedLogic.BookingDeposit as BookingDeposit
import qualified Storage.Queries.BookingPayment as QBookingPayment

data DepositLedgerView = DepositLedgerView
  { captured :: Bool,
    held :: Bool,
    everHeld :: Bool
  }

-- | Every attempt row counts, since the refund pays back every charged order, not just the latest.
--   Balance-funded deposits have no row: an ended booking that ever held one got it back in the balance.
--   A live unheld deposit is PAYABLE here; in-flight/failed attempts are resolved by the pay screen's status poll.
depositDisplayStatus :: DRB.BookingStatus -> DepositLedgerView -> [DBP.BookingPayment] -> API.BookingDepositStatus
depositDisplayStatus bookingStatus ledger rows
  | ledger.captured = API.FORFEITED
  | bookingStatus `elem` DRB.terminalBookingStatus = terminalStatus
  | ledger.held = API.COVERED
  | otherwise = API.PAYABLE
  where
    anyRow statuses = any ((`elem` statuses) . (.status)) rows
    terminalStatus
      | anyRow [DBP.REFUND_FAILED] = API.REFUND_FAILED
      | anyRow [DBP.REFUND_PENDING, DBP.REFUND_INITIATED] = API.REFUND_IN_PROGRESS
      | anyRow [DBP.REFUNDED] = API.REFUNDED
      | ledger.everHeld = API.RETURNED_TO_WALLET
      | otherwise = API.NOT_PAID

-- | Reallocated bookings are skipped: the rekey moved their hold and rows to the new booking.
withBookingDeposit :: DRB.Booking -> SRB.BookingAPIEntity -> Flow SRB.BookingAPIEntity
withBookingDeposit booking entity = case booking.bookingDepositAmount of
  Just amount | booking.status /= DRB.REALLOCATED -> do
    entries <- Ledger.getEntriesByReference BookingDeposit.bookingDepositHoldRefType booking.id.getId
    rows <- QBookingPayment.findAllByBookingIdAndServiceType booking.id DOrder.BookingDeposit
    let ledger =
          DepositLedgerView
            { captured = any ((== LE.SETTLED) . (.status)) entries,
              held = any ((== LE.PENDING) . (.status)) entries,
              everHeld = not (null entries)
            }
        refundedRows = filter ((`elem` [DBP.REFUND_PENDING, DBP.REFUND_INITIATED, DBP.REFUND_FAILED, DBP.REFUNDED]) . (.status)) rows
    refunds <- concat <$> mapM (\row -> QPaymentOrder.findById row.paymentOrderId >>= SRB.getOrderRefunds) refundedRows
    pure entity {SRB.bookingDeposit = Just (SRB.BookingDepositInfo amount (depositDisplayStatus booking.status ledger rows) refunds)}
  _ -> pure entity
