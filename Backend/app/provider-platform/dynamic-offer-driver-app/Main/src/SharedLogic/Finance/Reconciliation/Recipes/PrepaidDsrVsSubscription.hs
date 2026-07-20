{-
  Prepaid Subscription :: DSR ↔ SubscriptionCredit ledger.

  For every COMPLETED booking in the chunk:
    * expected = ride fare - parking - govt charges - toll
    * actual   = first ledger entry with reference_type = 'RideSubscriptionDebit'
                 and reference_id = booking.id

  Ports doReconciliationDsrVsSubscription + processDsrVsSubscription from the
  retired SharedLogic.Finance.Reconciliation module.
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.Reconciliation.Recipes.PrepaidDsrVsSubscription
  ( recipe,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Time (nominalDay)
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.FareParameters as DFP
import qualified Domain.Types.Ride as DR
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.LedgerEntry as LedgerEntry
import Lib.Finance.Reconciliation.Recipe (Recipe (..), defaultClassify)
import qualified Lib.Finance.Reconciliation.Types as ReconT
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.LedgerEntryExtra as QLedgerExtra
import SharedLogic.Finance.Prepaid (prepaidRideDebitReferenceType)
import qualified SharedLogic.Finance.Reconciliation.EntitySync as EntitySync
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.FareParameters as QFareParams
import qualified Storage.Queries.Ride as QRide

recipe ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Recipe m
recipe =
  Recipe
    { spec = mySpec,
      chunkPlan = ReconT.ByHour,
      -- Internal-only recon: ledger debit happens inline with the ride.
      settlementBuffer = nominalDay,
      grouping = ReconT.Individual,
      fetchSourceChunk = fetchSources,
      fetchTargetsById = fetchTargets,
      -- TODO: sweep re-fetch (SourceRecord rebuild would need bookings +
      -- rides + fareParams by id — deferred; force-close on age still works).
      fetchSourcesByIds = \_ _ -> pure [],
      sweepInterval = 4 * nominalDay,
      maxOpenAge = 30 * nominalDay,
      fetchOrphanTargets = Nothing,
      classify = defaultClassify,
      syncSourceStatus = Just (EntitySync.syncBookingStatus mySpec)
    }
  where
    mySpec = ReconT.ReconciliationSpec ReconT.PREPAID_SUBSCRIPTION ReconT.DSR ReconT.SUBSCRIPTION_PURCHASE

fetchSources ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconT.MerchantScope ->
  ReconT.DateRange ->
  m [ReconT.SourceRecord]
fetchSources scope range = do
  -- Query 1: bookings started in this chunk that reached COMPLETED.
  bookings <-
    QBooking.findAllByStatusAndDateRange
      (Id scope.merchantOperatingCityId)
      [DB.COMPLETED]
      range.from
      range.to

  -- Query 2: bulk-fetch every driver-ride for these bookings, in one query.
  allRides <- QRide.findRidesByBookingId (map (.id) bookings)
  let ridesByBookingId :: HM.HashMap Text DR.Ride
      ridesByBookingId = HM.fromList [(r.bookingId.getId, r) | r <- allRides]

  -- Query 3: bulk-fetch every fare_params row referenced by those rides.
  let fareParamIds = mapMaybe (.fareParametersId) allRides
  allFareParams <- QFareParams.findAllIn fareParamIds
  let fareParamsById :: HM.HashMap Text DFP.FareParameters
      fareParamsById = HM.fromList [(fp.id.getId, fp) | fp <- allFareParams]

  pure $
    flip map bookings $ \booking ->
      let mbRide = HM.lookup booking.id.getId ridesByBookingId
          dcoId = (\r -> r.driverId.getId) <$> mbRide
          mbRideFareParams = mbRide >>= (.fareParametersId) >>= \fpId -> HM.lookup fpId.getId fareParamsById
          fareParams = fromMaybe booking.fareParams mbRideFareParams
          rideFare = fromMaybe booking.estimatedFare (mbRide >>= (.fare))
          parkingCharge = fromMaybe 0 fareParams.parkingCharge
          govtCharges = fromMaybe 0 fareParams.govtCharges
          tollCharges = fromMaybe 0 $ (mbRide >>= (.tollCharges)) <|> booking.tollCharges
          expected = rideFare - parkingCharge - govtCharges - tollCharges
          bId = booking.id.getId
       in ReconT.SourceRecord
            { srcId = bId,
              srcEntityId = Just bId,
              srcPartyId = dcoId,
              srcAmount = expected,
              srcMatchKey = Just bId, -- ledger.reference_id
              srcComponent = Just "DRIVER_TAKE_HOME_EARNINGS",
              srcMeta =
                Just $
                  A.object
                    [ "bookingStatus" .= booking.status,
                      "rideFare" .= rideFare,
                      "parkingCharge" .= parkingCharge,
                      "govtCharges" .= govtCharges,
                      "tollCharges" .= tollCharges
                    ],
              srcTimestamp = booking.createdAt,
              srcLifecycle = ReconT.Settled -- COMPLETED bookings are terminal.
            }

fetchTargets ::
  ( BeamFlow m r,
    MonadFlow m
  ) =>
  ReconT.MerchantScope ->
  HS.HashSet Text ->
  m [ReconT.TargetRecord]
fetchTargets _scope bookingIds = do
  -- Single indexed WHERE reference_type = ? AND reference_id = ANY(?).
  entries <-
    QLedgerExtra.findByReferenceTypesAndReferenceIds
      [prepaidRideDebitReferenceType]
      (HS.toList bookingIds)
  -- Keep the first ledger entry per booking (old code took listToMaybe of
  -- the returned list; preserve that behaviour).
  let firstByBooking :: HM.HashMap Text LedgerEntry.LedgerEntry
      firstByBooking =
        HM.fromListWith (\_new old -> old) [(e.referenceId, e) | e <- entries]
  pure
    [ ReconT.TargetRecord
        { tgtId = e.id.getId, -- ledger row PK, stored as targetRecordId
          tgtMatchKey = e.referenceId, -- == booking.id, joined against srcMatchKey
          tgtAmount = e.amount,
          tgtMeta = Just $ A.object ["referenceType" .= (e.referenceType :: Text)],
          tgtSettlementId = Nothing,
          tgtSettlementDate = Nothing,
          tgtSettlementMode = Nothing,
          tgtRrn = Nothing,
          tgtTransactionDate = Just e.createdAt
        }
      | (_, e) <- HM.toList firstByBooking
    ]
