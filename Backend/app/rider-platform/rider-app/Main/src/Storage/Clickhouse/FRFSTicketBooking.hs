module Storage.Clickhouse.FRFSTicketBooking where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Quote as DQuote
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Common
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

chunkList :: Int -> [a] -> [[a]]
chunkList chunkSize
  | chunkSize <= 0 = (: [])
  | otherwise = go
  where
    go [] = []
    go xs =
      let (h, t) = splitAt chunkSize xs
       in h : go t

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

  -- 1. Fetch Booking Data (only required fields)
  bookings <-
    CH.findAll $
      CH.select_ (\b -> CH.notGrouped (b.bookingId, b.finalPrice, b.price, b.quantity, b.childTicketQuantity, b.vehicleType, b.quoteId)) $
        CH.filter_
          ( \b ->
              b.status CH.==. "CONFIRMED"
                CH.&&. b.merchantOperatingCityId CH.==. merchantOpCityId
                CH.&&. b.createdAt >=. startOfDayUTC
                CH.&&. b.createdAt <. endOfDayUTC
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fRFSTicketBookingTTable)

  -- 2. Fetch Quote Categories
  let quoteIds = Set.toList $ Set.fromList [qId | (_, _, _, _, _, _, qId) <- bookings]
      quoteIdBatches = chunkList 500 quoteIds
  quoteCategories <-
    concat
      <$> mapM
        (\batch -> QCategory.getQuoteCategoriesByQuoteIds batch startOfDayUTC endOfDayUTC)
        quoteIdBatches

  -- 3. Aggregate Quote Categories in Memory
  let quoteMap =
        Map.fromListWith
          (+)
          [ (qid, fromMaybe 0 selQty)
            | (qid, selQty) <- quoteCategories
          ]

  -- 4. Aggregate Bookings in Memory
  let initialMap = Map.empty -- Map VehicleType (SumPrice, Set BookingId, SumTickets)
      processBooking acc (bId, finalP, priceP, qty, childQty', vType, qId) =
        let
            effectivePrice =
              case finalP of
                Just v | v > 0 -> Just v
                _ -> priceP
            priceVal = maybe 0 (realToFrac . toRational) effectivePrice

            -- Quantity Logic
            quoteTotal = Map.findWithDefault 0 qId quoteMap

            adultQty = fromMaybe 0 qty
            childQty = fromMaybe 0 childQty'
            bookingTotal = adultQty + childQty

            ticketsCount = if bookingTotal > 0 then bookingTotal else quoteTotal

            (curPrice, curIds, curTickets) = Map.findWithDefault (0, Set.empty, 0) vType acc
         in Map.insert vType (curPrice + priceVal, Set.insert bId curIds, curTickets + ticketsCount) acc

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
