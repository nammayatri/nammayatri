-- | Finds the FRFS payment orders a CCAvenue split payout may still be owed for.
--
-- Conceptually this is the booking -> booking payment -> payment order join the manual
-- @scripts/frfs_split_pipeline.py@ ran in ClickHouse. The typed ClickhouseV2 DSL has no
-- join, so it is done as three filtered selects with the ids carried between them in
-- memory, the same way 'Storage.Clickhouse.FRFSTicketBooking' pairs bookings with quote
-- categories.
--
-- It returns only order short ids. Whether a split was already sent for one is decided
-- against @payment_transaction@ in Postgres, not here, because that is where the response
-- is recorded.
module Storage.Clickhouse.FRFSSplitPayoutCandidate
  ( findSettledOrderShortIds,
  )
where

import qualified Data.List as List
import qualified Data.Text as T
import Data.Time.Calendar (addDays)
import Data.Time.Clock (UTCTime (UTCTime), utctDay)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Clickhouse.FRFSTicketBooking (chunkList, fRFSTicketBookingTTable)
import qualified Storage.Clickhouse.FRFSTicketBookingPayment as CHBookingPayment
import qualified Storage.Clickhouse.PaymentOrder as CHPaymentOrder

bookingStatuses :: [Text]
bookingStatuses =
  [ "CONFIRMED",
    "CANCELLED",
    "CANCEL_INITIATED",
    "CONFIRMING",
    "COUNTER_CANCELLED",
    "REFUND_INITIATED"
  ]

defaultIdChunkSize :: Int
defaultIdChunkSize = 1000

findSettledOrderShortIds ::
  ( CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m,
    MonadFlow m
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Int ->
  Maybe Int ->
  m [Text]
findSettledOrderShortIds merchantOperatingCityId lookbackDays mbPageSize = do
  now <- getCurrentTime
  let since = UTCTime (addDays (negate . fromIntegral $ max 1 lookbackDays) (utctDay now)) 0

  let mbValidPageSize = case mbPageSize of
        Just pageSize | pageSize > 0 -> Just pageSize
        _ -> Nothing
      idChunkSize = fromMaybe defaultIdChunkSize mbValidPageSize

  bookingIds <-
    List.nub
      <$> maybe
        (fetchAllBookingIds merchantOperatingCityId since)
        (\pageSize -> fetchBookingIdsPaged merchantOperatingCityId since pageSize 0 [])
        mbValidPageSize

  paymentOrderIds <-
    fmap (List.nub . concat) $
      mapM
        CHBookingPayment.getPaymentOrderIdsByBookingIds
        (chunkList idChunkSize bookingIds)

  shortIds <-
    fmap (List.nub . concat) $
      mapM
        CHPaymentOrder.getShortIdsByOrderIds
        (chunkList idChunkSize paymentOrderIds)

  let candidates = filter (not . T.null) shortIds
  logInfo $
    "FRFS split payout candidates: " <> show (length bookingIds) <> " booking(s) -> "
      <> show (length paymentOrderIds)
      <> " order(s) -> "
      <> show (length candidates)
      <> " short id(s) since "
      <> show since
      <> " for city "
      <> merchantOperatingCityId.getId
  pure candidates

fetchBookingIdsPaged ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  Int ->
  Int ->
  [Text] ->
  m [Text]
fetchBookingIdsPaged merchantOperatingCityId since pageSize pageNo acc = do
  page <-
    map fst
      <$> ( CH.findAll $
              CH.select_ (\b -> CH.notGrouped (b.bookingId, b.createdAt)) $
                CH.orderBy_ (\_ (_, createdAt) -> CH.asc createdAt) $
                  CH.limit_ pageSize $
                    CH.offset_ (pageNo * pageSize) $
                      CH.filter_
                        ( \b ->
                            b.status `CH.in_` bookingStatuses
                              CH.&&. b.merchantOperatingCityId CH.==. merchantOperatingCityId
                              CH.&&. b.createdAt CH.>=. since
                        )
                        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fRFSTicketBookingTTable)
          )
  let acc' = acc <> page
  if length page < pageSize
    then pure acc'
    else fetchBookingIdsPaged merchantOperatingCityId since pageSize (pageNo + 1) acc'

fetchAllBookingIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  m [Text]
fetchAllBookingIds merchantOperatingCityId since =
  map fst
    <$> ( CH.findAll $
            CH.select_ (\b -> CH.notGrouped (b.bookingId, b.createdAt)) $
              CH.filter_
                ( \b ->
                    b.status `CH.in_` bookingStatuses
                      CH.&&. b.merchantOperatingCityId CH.==. merchantOperatingCityId
                      CH.&&. b.createdAt CH.>=. since
                )
                (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fRFSTicketBookingTTable)
        )
