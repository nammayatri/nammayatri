{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.NyRegularSubscription (module Storage.Queries.NyRegularSubscription, module ReExport) where

import qualified Data.Aeson
import qualified Data.Time
import qualified Data.Time.Calendar
import qualified Data.Time.LocalTime
import qualified Domain.Types.Extra.NyRegularSubscription
import qualified Domain.Types.NyRegularSubscription
import qualified Domain.Types.Person
import qualified Domain.Types.ServiceTierType
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.NyRegularSubscription as Beam
import Storage.Queries.NyRegularSubscriptionExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.NyRegularSubscription.NyRegularSubscription -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.NyRegularSubscription.NyRegularSubscription] -> m ())
createMany = traverse_ create

confirmSubscriptionDetailsById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType -> Kernel.Prelude.Maybe Kernel.Types.Common.Price -> Kernel.Prelude.Maybe Data.Aeson.Value -> Kernel.Prelude.Maybe Data.Time.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.Extra.NyRegularSubscription.NyRegularSubscriptionStatus -> Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription -> m ())
confirmSubscriptionDetailsById vehicleServiceTier fixedPrice fixedPriceBreakupDetails fixedPriceExpiryDate initialBppQuoteId status id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.vehicleServiceTier vehicleServiceTier,
      Se.Set Beam.fixedPrice ((.amount) <$> fixedPrice),
      Se.Set Beam.fixedPriceCurrency ((.currency) <$> fixedPrice),
      Se.Set Beam.fixedPriceBreakupDetails fixedPriceBreakupDetails,
      Se.Set Beam.fixedPriceExpiryDate fixedPriceExpiryDate,
      Se.Set Beam.initialBppQuoteId initialBppQuoteId,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription -> m (Maybe Domain.Types.NyRegularSubscription.NyRegularSubscription))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByUserId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.NyRegularSubscription.NyRegularSubscription])
findByUserId limit offset userId = do findAllWithOptionsKV [Se.Is Beam.userId $ Se.Eq (Kernel.Types.Id.getId userId)] (Se.Desc Beam.createdAt) limit offset

updateLastProcessedAtById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Data.Time.UTCTime -> Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription -> m ())
updateLastProcessedAtById lastProcessedAt id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.lastProcessedAt lastProcessedAt, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updatePauseDetailsById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Data.Time.UTCTime -> Kernel.Prelude.Maybe Data.Time.UTCTime -> Domain.Types.Extra.NyRegularSubscription.NyRegularSubscriptionStatus -> Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription -> m ())
updatePauseDetailsById pauseStartDate pauseEndDate status id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.pauseStartDate pauseStartDate,
      Se.Set Beam.pauseEndDate pauseEndDate,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateRecurrenceById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ([Data.Time.Calendar.DayOfWeek] -> Data.Time.LocalTime.TimeOfDay -> Kernel.Prelude.Maybe Data.Time.Calendar.Day -> Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription -> m ())
updateRecurrenceById recurrenceRuleDays scheduledTimeOfDay recurrenceEndDate id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.recurrenceRuleDays (map show recurrenceRuleDays),
      Se.Set Beam.scheduledTimeOfDay scheduledTimeOfDay,
      Se.Set Beam.recurrenceEndDate recurrenceEndDate,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateSchedulingHashById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription -> m ())
updateSchedulingHashById schedulingHash id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.schedulingHash schedulingHash, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.Extra.NyRegularSubscription.NyRegularSubscriptionStatus -> Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription -> m ())
updateStatusById status id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription -> m (Maybe Domain.Types.NyRegularSubscription.NyRegularSubscription))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.NyRegularSubscription.NyRegularSubscription -> m ())
updateByPrimaryKey (Domain.Types.NyRegularSubscription.NyRegularSubscription {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bppId bppId,
      Se.Set Beam.dropoffLocationId (Kernel.Types.Id.getId $ (.id) dropoffLocation),
      Se.Set Beam.fixedPrice ((.amount) <$> fixedPrice),
      Se.Set Beam.fixedPriceCurrency ((.currency) <$> fixedPrice),
      Se.Set Beam.fixedPriceBreakupDetails fixedPriceBreakupDetails,
      Se.Set Beam.fixedPriceExpiryDate fixedPriceExpiryDate,
      Se.Set Beam.initialBppQuoteId initialBppQuoteId,
      Se.Set Beam.lastProcessedAt lastProcessedAt,
      Se.Set Beam.metadata metadata,
      Se.Set Beam.pauseEndDate pauseEndDate,
      Se.Set Beam.pauseStartDate pauseStartDate,
      Se.Set Beam.pickupLocationId (Kernel.Types.Id.getId $ (.id) pickupLocation),
      Se.Set Beam.recurrenceEndDate recurrenceEndDate,
      Se.Set Beam.recurrenceRuleDays (map show recurrenceRuleDays),
      Se.Set Beam.scheduledTimeOfDay scheduledTimeOfDay,
      Se.Set Beam.schedulingHash schedulingHash,
      Se.Set Beam.startDatetime startDatetime,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.userId (Kernel.Types.Id.getId userId),
      Se.Set Beam.vehicleServiceTier vehicleServiceTier,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
