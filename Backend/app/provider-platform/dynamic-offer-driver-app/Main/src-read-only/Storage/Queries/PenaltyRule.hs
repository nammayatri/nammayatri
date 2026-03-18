{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PenaltyRule (module Storage.Queries.PenaltyRule, module ReExport) where

import qualified Domain.Types.Merchant
import qualified Domain.Types.PenaltyRule
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PenaltyRule as Beam
import Storage.Queries.PenaltyRuleExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PenaltyRule.PenaltyRule -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PenaltyRule.PenaltyRule] -> m ())
createMany = traverse_ create

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.PenaltyRule.PenaltyRule -> m (Maybe Domain.Types.PenaltyRule.PenaltyRule))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findAllByMerchantIdAndTriggerEvent ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Domain.Types.PenaltyRule.PenaltyTriggerEvent -> Kernel.Prelude.Bool -> m [Domain.Types.PenaltyRule.PenaltyRule])
findAllByMerchantIdAndTriggerEvent merchantId triggerEvent isActive = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.triggerEvent $ Se.Eq triggerEvent,
          Se.Is Beam.isActive $ Se.Eq isActive
        ]
    ]

findAllByMerchantId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m [Domain.Types.PenaltyRule.PenaltyRule])
findAllByMerchantId merchantId = do findAllWithKV [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]

updateIsActiveById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Bool -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.PenaltyRule.PenaltyRule -> m ())
updateIsActiveById isActive updatedAt id = do
  updateWithKV
    [ Se.Set Beam.isActive isActive,
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.PenaltyRule.PenaltyRule -> m (Maybe Domain.Types.PenaltyRule.PenaltyRule))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PenaltyRule.PenaltyRule -> m ())
updateByPrimaryKey (Domain.Types.PenaltyRule.PenaltyRule {..}) = do
  updateWithKV
    [ Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.triggerEvent triggerEvent,
      Se.Set Beam.conditionsJson conditionsJson,
      Se.Set Beam.penaltyType penaltyType,
      Se.Set Beam.fixedAmount fixedAmount,
      Se.Set Beam.percentage percentage,
      Se.Set Beam.formulaExpression formulaExpression,
      Se.Set Beam.currency (Kernel.Prelude.Just currency),
      Se.Set Beam.gracePeriodCount gracePeriodCount,
      Se.Set Beam.gracePeriodWindowHours gracePeriodWindowHours,
      Se.Set Beam.priority priority,
      Se.Set Beam.isActive isActive,
      Se.Set Beam.startDate startDate,
      Se.Set Beam.endDate endDate,
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PenaltyRule Domain.Types.PenaltyRule.PenaltyRule where
  fromTType' (Beam.PenaltyRuleT {..}) = do
    pure $
      Just
        Domain.Types.PenaltyRule.PenaltyRule
          { id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            triggerEvent = triggerEvent,
            conditionsJson = conditionsJson,
            penaltyType = penaltyType,
            fixedAmount = fixedAmount,
            percentage = percentage,
            formulaExpression = formulaExpression,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            gracePeriodCount = gracePeriodCount,
            gracePeriodWindowHours = gracePeriodWindowHours,
            priority = priority,
            isActive = isActive,
            startDate = startDate,
            endDate = endDate,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PenaltyRule Domain.Types.PenaltyRule.PenaltyRule where
  toTType' (Domain.Types.PenaltyRule.PenaltyRule {..}) = do
    Beam.PenaltyRuleT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.triggerEvent = triggerEvent,
        Beam.conditionsJson = conditionsJson,
        Beam.penaltyType = penaltyType,
        Beam.fixedAmount = fixedAmount,
        Beam.percentage = percentage,
        Beam.formulaExpression = formulaExpression,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.gracePeriodCount = gracePeriodCount,
        Beam.gracePeriodWindowHours = gracePeriodWindowHours,
        Beam.priority = priority,
        Beam.isActive = isActive,
        Beam.startDate = startDate,
        Beam.endDate = endDate,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
