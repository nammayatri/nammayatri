{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.OfferFrequencyStatsHistory where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.Offer
import qualified Lib.Payment.Domain.Types.OfferFrequencyStatsHistory
import qualified Lib.Payment.Domain.Types.OfferStats
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.OfferFrequencyStatsHistory as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.OfferFrequencyStatsHistory.OfferFrequencyStatsHistory -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.OfferFrequencyStatsHistory.OfferFrequencyStatsHistory] -> m ())
createMany = traverse_ create

findAllByEntityIdEntityTypeAndOfferId ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> Lib.Payment.Domain.Types.OfferStats.OfferStatsEntityType -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer -> m [Lib.Payment.Domain.Types.OfferFrequencyStatsHistory.OfferFrequencyStatsHistory])
findAllByEntityIdEntityTypeAndOfferId entityId entityType offerId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.entityId $ Se.Eq entityId,
          Se.Is Beam.entityType $ Se.Eq entityType,
          Se.Is Beam.offerId $ Se.Eq (Kernel.Types.Id.getId offerId)
        ]
    ]

findByEntityIdEntityTypeOfferIdFrequencyTypeAndPeriodStart ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> Lib.Payment.Domain.Types.OfferStats.OfferStatsEntityType -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer -> Lib.Payment.Domain.Types.Offer.OfferFrequency -> Kernel.Prelude.UTCTime -> m (Maybe Lib.Payment.Domain.Types.OfferFrequencyStatsHistory.OfferFrequencyStatsHistory))
findByEntityIdEntityTypeOfferIdFrequencyTypeAndPeriodStart entityId entityType offerId frequencyType periodStart = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.entityId $ Se.Eq entityId,
          Se.Is Beam.entityType $ Se.Eq entityType,
          Se.Is Beam.offerId $ Se.Eq (Kernel.Types.Id.getId offerId),
          Se.Is Beam.frequencyType $ Se.Eq frequencyType,
          Se.Is Beam.periodStart $ Se.Eq periodStart
        ]
    ]

findByPrimaryKey ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.OfferFrequencyStatsHistory.OfferFrequencyStatsHistory -> m (Maybe Lib.Payment.Domain.Types.OfferFrequencyStatsHistory.OfferFrequencyStatsHistory))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.OfferFrequencyStatsHistory.OfferFrequencyStatsHistory -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.OfferFrequencyStatsHistory.OfferFrequencyStatsHistory {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.appliedCount appliedCount,
      Se.Set Beam.currency currency,
      Se.Set Beam.entityId entityId,
      Se.Set Beam.entityType entityType,
      Se.Set Beam.frequencyType frequencyType,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.offerId (Kernel.Types.Id.getId offerId),
      Se.Set Beam.periodEnd periodEnd,
      Se.Set Beam.periodStart periodStart,
      Se.Set Beam.totalCashbackAmount totalCashbackAmount,
      Se.Set Beam.totalDiscountAmount totalDiscountAmount,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.OfferFrequencyStatsHistory Lib.Payment.Domain.Types.OfferFrequencyStatsHistory.OfferFrequencyStatsHistory where
  fromTType' (Beam.OfferFrequencyStatsHistoryT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.OfferFrequencyStatsHistory.OfferFrequencyStatsHistory
          { appliedCount = appliedCount,
            createdAt = createdAt,
            currency = currency,
            entityId = entityId,
            entityType = entityType,
            frequencyType = frequencyType,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            offerId = Kernel.Types.Id.Id offerId,
            periodEnd = periodEnd,
            periodStart = periodStart,
            totalCashbackAmount = totalCashbackAmount,
            totalDiscountAmount = totalDiscountAmount,
            updatedAt = updatedAt
          }

instance ToTType' Beam.OfferFrequencyStatsHistory Lib.Payment.Domain.Types.OfferFrequencyStatsHistory.OfferFrequencyStatsHistory where
  toTType' (Lib.Payment.Domain.Types.OfferFrequencyStatsHistory.OfferFrequencyStatsHistory {..}) = do
    Beam.OfferFrequencyStatsHistoryT
      { Beam.appliedCount = appliedCount,
        Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.entityId = entityId,
        Beam.entityType = entityType,
        Beam.frequencyType = frequencyType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.offerId = Kernel.Types.Id.getId offerId,
        Beam.periodEnd = periodEnd,
        Beam.periodStart = periodStart,
        Beam.totalCashbackAmount = totalCashbackAmount,
        Beam.totalDiscountAmount = totalDiscountAmount,
        Beam.updatedAt = updatedAt
      }
