module Storage.Clickhouse.FRFSTicketBooking where

import Data.Time
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Common
import Kernel.Types.Id

data FRFSTicketBookingT f = FRFSTicketBookingT
  { bookingId :: C f Text,
    finalPrice :: C f (Maybe HighPrecMoney),
    quantity :: C f (Maybe Int),
    childTicketQuantity :: C f (Maybe Int),
    vehicleType :: C f Text,
    status :: C f Text,
    merchantOperatingCityId :: C f (Id DMOC.MerchantOperatingCity),
    createdAt :: C f UTCTime
  }
  deriving (Generic)

deriving instance Show FRFSTicketBooking

fRFSTicketBookingTTable :: FRFSTicketBookingT (FieldModification FRFSTicketBookingT)
fRFSTicketBookingTTable =
  FRFSTicketBookingT
    { bookingId = "id",
      finalPrice = "final_price",
      quantity = "quantity",
      childTicketQuantity = "child_ticket_quantity",
      vehicleType = "vehicle_type",
      status = "status",
      merchantOperatingCityId = "merchant_operating_city_id",
      createdAt = "created_at"
    }

type FRFSTicketBooking = FRFSTicketBookingT Identity

$(TH.mkClickhouseInstances ''FRFSTicketBookingT 'NO_SELECT_MODIFIER)

-- Aggregated booking metrics result
data BookingMetrics = BookingMetrics
  { metricsVehicleType :: Text,
    metricsTotalPrice :: Maybe Double,
    metricsUniqueBookings :: Int,
    metricsTotalTickets :: Int
  }
  deriving (Generic, Show)

getBookingMetricsByDateRange ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DMOC.MerchantOperatingCity ->
  Day -> -- Target date for IST
  m [BookingMetrics]
getBookingMetricsByDateRange merchantOpCityId targetDate = do
  let istOffset = 19800 :: NominalDiffTime -- 5 hours 30 minutes in seconds
      startOfDayUTC = addUTCTime (negate istOffset) (UTCTime targetDate 0)
      endOfDayUTC = addUTCTime (86400 - istOffset) (UTCTime targetDate 0) -- 24 hours later in IST
  results <-
    CH.findAll $
      CH.select_
        ( \booking ->
            CH.groupBy booking.vehicleType $ \vType ->
              let totalPrice = CH.sum_ booking.finalPrice
                  bookingCount = CH.count_ booking.bookingId
                  regularTickets = CH.sum_ booking.quantity
                  childTickets = CH.sum_ booking.childTicketQuantity
               in (vType, totalPrice, bookingCount, regularTickets, childTickets)
        )
        $ CH.filter_
          ( \booking ->
              booking.status CH.==. "CONFIRMED"
                CH.&&. booking.merchantOperatingCityId CH.==. merchantOpCityId
                CH.&&. booking.createdAt >=. startOfDayUTC
                CH.&&. booking.createdAt <. endOfDayUTC
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fRFSTicketBookingTTable)
  pure $
    map
      ( \(vType, totalPrice, count, regularTickets, childTickets) ->
          BookingMetrics
            { metricsVehicleType = vType,
              metricsTotalPrice = fmap (realToFrac . toRational) totalPrice,
              metricsUniqueBookings = count,
              metricsTotalTickets = fromMaybe 0 regularTickets + fromMaybe 0 childTickets
            }
      )
      results
