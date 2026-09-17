{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.PersonOfferFrequencyStats where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.Offer
import qualified Lib.Payment.Domain.Types.OfferStats
import qualified Lib.Payment.Domain.Types.PersonOfferFrequencyStats
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PersonOfferFrequencyStats as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PersonOfferFrequencyStats.PersonOfferFrequencyStats -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.PersonOfferFrequencyStats.PersonOfferFrequencyStats] -> m ())
createMany = traverse_ create

findByEntityIdOfferIdEntityType ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer -> Lib.Payment.Domain.Types.OfferStats.OfferStatsEntityType -> m (Maybe Lib.Payment.Domain.Types.PersonOfferFrequencyStats.PersonOfferFrequencyStats))
findByEntityIdOfferIdEntityType entityId offerId entityType = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.entityId $ Se.Eq entityId,
          Se.Is Beam.offerId $ Se.Eq (Kernel.Types.Id.getId offerId),
          Se.Is Beam.entityType $ Se.Eq entityType
        ]
    ]

findByPrimaryKey ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PersonOfferFrequencyStats.PersonOfferFrequencyStats -> m (Maybe Lib.Payment.Domain.Types.PersonOfferFrequencyStats.PersonOfferFrequencyStats))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PersonOfferFrequencyStats.PersonOfferFrequencyStats -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.PersonOfferFrequencyStats.PersonOfferFrequencyStats {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.appliedCount appliedCount,
      Se.Set Beam.currency currency,
      Se.Set Beam.entityId entityId,
      Se.Set Beam.entityType entityType,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.offerId (Kernel.Types.Id.getId offerId),
      Se.Set Beam.periodStart periodStart,
      Se.Set Beam.totalCashbackAmount totalCashbackAmount,
      Se.Set Beam.totalDiscountAmount totalDiscountAmount,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PersonOfferFrequencyStats Lib.Payment.Domain.Types.PersonOfferFrequencyStats.PersonOfferFrequencyStats where
  fromTType' (Beam.PersonOfferFrequencyStatsT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.PersonOfferFrequencyStats.PersonOfferFrequencyStats
          { appliedCount = appliedCount,
            createdAt = createdAt,
            currency = currency,
            entityId = entityId,
            entityType = entityType,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            offerId = Kernel.Types.Id.Id offerId,
            periodStart = periodStart,
            totalCashbackAmount = totalCashbackAmount,
            totalDiscountAmount = totalDiscountAmount,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PersonOfferFrequencyStats Lib.Payment.Domain.Types.PersonOfferFrequencyStats.PersonOfferFrequencyStats where
  toTType' (Lib.Payment.Domain.Types.PersonOfferFrequencyStats.PersonOfferFrequencyStats {..}) = do
    Beam.PersonOfferFrequencyStatsT
      { Beam.appliedCount = appliedCount,
        Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.entityId = entityId,
        Beam.entityType = entityType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.offerId = Kernel.Types.Id.getId offerId,
        Beam.periodStart = periodStart,
        Beam.totalCashbackAmount = totalCashbackAmount,
        Beam.totalDiscountAmount = totalDiscountAmount,
        Beam.updatedAt = updatedAt
      }
