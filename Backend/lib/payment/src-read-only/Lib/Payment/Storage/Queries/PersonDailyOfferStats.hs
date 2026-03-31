{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.PersonDailyOfferStats where

import qualified Data.Time.Calendar
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.Common
import qualified Lib.Payment.Domain.Types.PersonDailyOfferStats
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PersonDailyOfferStats as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PersonDailyOfferStats.PersonDailyOfferStats -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.PersonDailyOfferStats.PersonDailyOfferStats] -> m ())
createMany = traverse_ create

findAllByDate :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Data.Time.Calendar.Day -> m ([Lib.Payment.Domain.Types.PersonDailyOfferStats.PersonDailyOfferStats]))
findAllByDate date = do findAllWithKV [Se.Is Beam.date $ Se.Eq date]

findAllByDateAndPayoutStatus ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Data.Time.Calendar.Day -> Lib.Payment.Domain.Types.Common.PayoutStatus -> m ([Lib.Payment.Domain.Types.PersonDailyOfferStats.PersonDailyOfferStats]))
findAllByDateAndPayoutStatus date payoutStatus = do findAllWithKV [Se.And [Se.Is Beam.date $ Se.Eq date, Se.Is Beam.payoutStatus $ Se.Eq payoutStatus]]

findByPersonIdAndDate ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m (Maybe Lib.Payment.Domain.Types.PersonDailyOfferStats.PersonDailyOfferStats))
findByPersonIdAndDate personId date = do findOneWithKV [Se.And [Se.Is Beam.personId $ Se.Eq personId, Se.Is Beam.date $ Se.Eq date]]

findByPrimaryKey ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PersonDailyOfferStats.PersonDailyOfferStats -> m (Maybe Lib.Payment.Domain.Types.PersonDailyOfferStats.PersonDailyOfferStats))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PersonDailyOfferStats.PersonDailyOfferStats -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.PersonDailyOfferStats.PersonDailyOfferStats {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.currency currency,
      Se.Set Beam.date date,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.offerCount offerCount,
      Se.Set Beam.payoutStatus payoutStatus,
      Se.Set Beam.personId personId,
      Se.Set Beam.totalCashbackAmount totalCashbackAmount,
      Se.Set Beam.totalDiscountAmount totalDiscountAmount,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PersonDailyOfferStats Lib.Payment.Domain.Types.PersonDailyOfferStats.PersonDailyOfferStats where
  fromTType' (Beam.PersonDailyOfferStatsT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.PersonDailyOfferStats.PersonDailyOfferStats
          { createdAt = createdAt,
            currency = currency,
            date = date,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            offerCount = offerCount,
            payoutStatus = payoutStatus,
            personId = personId,
            totalCashbackAmount = totalCashbackAmount,
            totalDiscountAmount = totalDiscountAmount,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PersonDailyOfferStats Lib.Payment.Domain.Types.PersonDailyOfferStats.PersonDailyOfferStats where
  toTType' (Lib.Payment.Domain.Types.PersonDailyOfferStats.PersonDailyOfferStats {..}) = do
    Beam.PersonDailyOfferStatsT
      { Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.date = date,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.offerCount = offerCount,
        Beam.payoutStatus = payoutStatus,
        Beam.personId = personId,
        Beam.totalCashbackAmount = totalCashbackAmount,
        Beam.totalDiscountAmount = totalDiscountAmount,
        Beam.updatedAt = updatedAt
      }
