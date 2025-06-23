{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.NyRegularSubscription where

import qualified Data.Aeson
import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Data.Time.LocalTime
import qualified Domain.Types.NyRegularSubscription
import qualified Domain.Types.Person
import qualified Domain.Types.ServiceTierType
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.NyRegularSubscription as Beam
import qualified Storage.Queries.Location

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.NyRegularSubscription.NyRegularSubscription -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.NyRegularSubscription.NyRegularSubscription] -> m ())
createMany = traverse_ create

confirmSubscriptionDetailsById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType -> Kernel.Prelude.Maybe Kernel.Types.Common.Price -> Kernel.Prelude.Maybe Data.Aeson.Value -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.NyRegularSubscription.NyRegularSubscriptionStatus -> Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription -> m ())
confirmSubscriptionDetailsById vehicleServiceTier fixedPrice fixedPriceBreakupDetails fixedPriceExpiryDate initialBppQuoteId status id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.vehicleServiceTier vehicleServiceTier,
      Se.Set Beam.fixedPrice (((.amount) <$> fixedPrice)),
      Se.Set Beam.fixedPriceCurrency (((.currency) <$> fixedPrice)),
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

findByUserId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.NyRegularSubscription.NyRegularSubscription]))
findByUserId userId = do findAllWithKV [Se.Is Beam.userId $ Se.Eq (Kernel.Types.Id.getId userId)]

updatePauseDetailsById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Domain.Types.NyRegularSubscription.NyRegularSubscriptionStatus -> Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription -> m ())
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

updateStatusById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.NyRegularSubscription.NyRegularSubscriptionStatus -> Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription -> m ())
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
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.dropoffLocationId (Kernel.Types.Id.getId $ (.id) dropoffLocation),
      Se.Set Beam.fixedPrice (((.amount) <$> fixedPrice)),
      Se.Set Beam.fixedPriceCurrency (((.currency) <$> fixedPrice)),
      Se.Set Beam.fixedPriceBreakupDetails fixedPriceBreakupDetails,
      Se.Set Beam.fixedPriceExpiryDate fixedPriceExpiryDate,
      Se.Set Beam.initialBppQuoteId initialBppQuoteId,
      Se.Set Beam.metadata metadata,
      Se.Set Beam.pauseEndDate pauseEndDate,
      Se.Set Beam.pauseStartDate pauseStartDate,
      Se.Set Beam.pickupLocationId (Kernel.Types.Id.getId $ (.id) pickupLocation),
      Se.Set Beam.recurrenceEndDate recurrenceEndDate,
      Se.Set Beam.recurrenceRuleDays (map show recurrenceRuleDays),
      Se.Set Beam.scheduledTimeOfDay scheduledTimeOfDay,
      Se.Set Beam.startDatetime startDatetime,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.userId (Kernel.Types.Id.getId userId),
      Se.Set Beam.vehicleServiceTier vehicleServiceTier,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.NyRegularSubscription Domain.Types.NyRegularSubscription.NyRegularSubscription where
  fromTType' (Beam.NyRegularSubscriptionT {..}) = do
    dropoffLocation' <- Storage.Queries.Location.findById (Kernel.Types.Id.Id dropoffLocationId) >>= fromMaybeM (Kernel.Types.Error.InternalError ("Failed to get dropoffLocation " <> dropoffLocationId))
    pickupLocation' <- Storage.Queries.Location.findById (Kernel.Types.Id.Id pickupLocationId) >>= fromMaybeM (Kernel.Types.Error.InternalError ("Failed to get pickupLocation " <> pickupLocationId))
    pure $
      Just
        Domain.Types.NyRegularSubscription.NyRegularSubscription
          { bppId = bppId,
            createdAt = createdAt,
            dropoffLocation = dropoffLocation',
            fixedPrice = (Kernel.Types.Common.mkPrice fixedPriceCurrency) <$> fixedPrice,
            fixedPriceBreakupDetails = fixedPriceBreakupDetails,
            fixedPriceExpiryDate = fixedPriceExpiryDate,
            id = Kernel.Types.Id.Id id,
            initialBppQuoteId = initialBppQuoteId,
            metadata = metadata,
            pauseEndDate = pauseEndDate,
            pauseStartDate = pauseStartDate,
            pickupLocation = pickupLocation',
            recurrenceEndDate = recurrenceEndDate,
            recurrenceRuleDays = ((read . Data.Text.unpack) <$> recurrenceRuleDays),
            scheduledTimeOfDay = scheduledTimeOfDay,
            startDatetime = startDatetime,
            status = status,
            updatedAt = updatedAt,
            userId = Kernel.Types.Id.Id userId,
            vehicleServiceTier = vehicleServiceTier,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.NyRegularSubscription Domain.Types.NyRegularSubscription.NyRegularSubscription where
  toTType' (Domain.Types.NyRegularSubscription.NyRegularSubscription {..}) = do
    Beam.NyRegularSubscriptionT
      { Beam.bppId = bppId,
        Beam.createdAt = createdAt,
        Beam.dropoffLocationId = Kernel.Types.Id.getId $ (.id) dropoffLocation,
        Beam.fixedPrice = ((.amount) <$> fixedPrice),
        Beam.fixedPriceCurrency = ((.currency) <$> fixedPrice),
        Beam.fixedPriceBreakupDetails = fixedPriceBreakupDetails,
        Beam.fixedPriceExpiryDate = fixedPriceExpiryDate,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.initialBppQuoteId = initialBppQuoteId,
        Beam.metadata = metadata,
        Beam.pauseEndDate = pauseEndDate,
        Beam.pauseStartDate = pauseStartDate,
        Beam.pickupLocationId = Kernel.Types.Id.getId $ (.id) pickupLocation,
        Beam.recurrenceEndDate = recurrenceEndDate,
        Beam.recurrenceRuleDays = map show recurrenceRuleDays,
        Beam.scheduledTimeOfDay = scheduledTimeOfDay,
        Beam.startDatetime = startDatetime,
        Beam.status = status,
        Beam.updatedAt = updatedAt,
        Beam.userId = Kernel.Types.Id.getId userId,
        Beam.vehicleServiceTier = vehicleServiceTier,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
