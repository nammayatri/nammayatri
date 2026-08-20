module Storage.Clickhouse.FRFSTicketBookingPayment where

import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH

data FRFSTicketBookingPaymentT f = FRFSTicketBookingPaymentT
  { frfsTicketBookingId :: C f Text,
    paymentOrderId :: C f Text,
    createdAt :: C f UTCTime
  }
  deriving (Generic)

deriving instance Show FRFSTicketBookingPayment

fRFSTicketBookingPaymentTTable :: FRFSTicketBookingPaymentT (FieldModification FRFSTicketBookingPaymentT)
fRFSTicketBookingPaymentTTable =
  FRFSTicketBookingPaymentT
    { frfsTicketBookingId = "frfs_ticket_booking_id",
      paymentOrderId = "payment_order_id",
      createdAt = "created_at"
    }

type FRFSTicketBookingPayment = FRFSTicketBookingPaymentT Identity

$(TH.mkClickhouseInstances ''FRFSTicketBookingPaymentT 'NO_SELECT_MODIFIER)

getPaymentOrderIdsByBookingIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Text] ->
  m [Text]
getPaymentOrderIdsByBookingIds bookingIds =
  CH.findAll $
    CH.select_ (\tbp -> CH.notGrouped tbp.paymentOrderId) $
      CH.selectModifierOverride CH.NO_SELECT_MODIFIER $
        CH.filter_
          (\tbp -> tbp.frfsTicketBookingId `CH.in_` bookingIds)
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fRFSTicketBookingPaymentTTable)
