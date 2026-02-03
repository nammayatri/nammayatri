module Storage.Clickhouse.FRFSTicketBooking where

import Data.Time
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Common
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Domain.Types.Quote as DQuote
import Control.Applicative ((<|>))
import Kernel.Types.Id
import qualified Storage.Clickhouse.FRFSQuoteCategory as QCategory

data FRFSTicketBookingT f = FRFSTicketBookingT
  { bookingId :: C f Text,
    finalPrice :: C f (Maybe HighPrecMoney),
    price :: C f (Maybe HighPrecMoney),
    quantity :: C f (Maybe Int),
    childTicketQuantity :: C f (Maybe Int),
    vehicleType :: C f Text,
    status :: C f Text,
    merchantOperatingCityId :: C f (Id DMOC.MerchantOperatingCity),
    createdAt :: C f UTCTime,
    quoteId :: C f (Id DQuote.Quote)
  }
  deriving (Generic)

deriving instance Show FRFSTicketBooking

fRFSTicketBookingTTable :: FRFSTicketBookingT (FieldModification FRFSTicketBookingT)
fRFSTicketBookingTTable =
  FRFSTicketBookingT
    { bookingId = "id",
      finalPrice = "final_price",
      price = "price",
      quantity = "quantity",
      childTicketQuantity = "child_ticket_quantity",
      vehicleType = "vehicle_type",
      status = "status",
      merchantOperatingCityId = "merchant_operating_city_id",
      createdAt = "created_at",
      quoteId = "quote_id"
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

  -- 1. Fetch Booking Data
  bookings <-
    CH.findAll $
      CH.select $
        CH.filter_
          ( \b ->
              b.status CH.==. "CONFIRMED"
                CH.&&. b.merchantOperatingCityId CH.==. merchantOpCityId
                CH.&&. b.createdAt >=. startOfDayUTC
                CH.&&. b.createdAt <. endOfDayUTC
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fRFSTicketBookingTTable)

  -- 2. Fetch Quote Categories
  let quoteIds = Set.toList $ Set.fromList [b.quoteId | b <- bookings]
  quoteCategories <- QCategory.getQuoteCategoriesByQuoteIds quoteIds

  -- 3. Aggregate Quote Categories in Memory
  let quoteMap =
        Map.fromListWith
          (+)
          [ (qc.quoteId, fromMaybe 0 qc.selectedQuantity)
            | qc <- quoteCategories
          ]

  -- 4. Aggregate Bookings in Memory
  let initialMap = Map.empty -- Map VehicleType (SumPrice, Set BookingId, SumTickets)
      processBooking acc b =
        let -- Price Logic: coalesce(finalPrice, price)
            finalP = b.finalPrice
            priceP = b.price
            effectivePrice = finalP <|> priceP
            priceVal = maybe 0 (realToFrac . toRational) effectivePrice

            -- Quantity Logic
            quoteTotal = Map.findWithDefault 0 b.quoteId quoteMap

            -- If booking has explicitly recorded quantities (even 0), use them.
            -- If both are missing (Nothing), fallback to the quote's total.
            ticketsCount = case (b.quantity, b.childTicketQuantity) of
              (Nothing, Nothing) -> quoteTotal
              _ -> fromMaybe 0 b.quantity + fromMaybe 0 b.childTicketQuantity

            (curPrice, curIds, curTickets) = Map.findWithDefault (0, Set.empty, 0) b.vehicleType acc
         in Map.insert b.vehicleType (curPrice + priceVal, Set.insert b.bookingId curIds, curTickets + ticketsCount) acc

      aggregated = List.foldl' processBooking initialMap bookings

  pure $
    map
      ( \(vType, (tPrice, ids, tickets)) ->
          BookingMetrics
            { metricsVehicleType = vType,
              metricsTotalPrice = Just tPrice,
              metricsUniqueBookings = Set.size ids,
              metricsTotalTickets = tickets
            }
      )
      (Map.toList aggregated)
