{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.ScheduledTransferRule where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.ScheduledTransferRule
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.ScheduledTransferRule as Beam
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.ScheduledTransferRule.ScheduledTransferRule -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.ScheduledTransferRule.ScheduledTransferRule] -> m ())
createMany = traverse_ create

findAllByCity :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m [Lib.Finance.Domain.Types.ScheduledTransferRule.ScheduledTransferRule])
findAllByCity merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

findById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.ScheduledTransferRule.ScheduledTransferRule -> m (Maybe Lib.Finance.Domain.Types.ScheduledTransferRule.ScheduledTransferRule))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByName :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Lib.Finance.Domain.Types.ScheduledTransferRule.ScheduledTransferRule))
findByName merchantOperatingCityId name = do findOneWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId, Se.Is Beam.name $ Se.Eq name]]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.ScheduledTransferRule.ScheduledTransferRule -> m (Maybe Lib.Finance.Domain.Types.ScheduledTransferRule.ScheduledTransferRule))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.ScheduledTransferRule.ScheduledTransferRule -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.ScheduledTransferRule.ScheduledTransferRule {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.accountType accountType,
      Se.Set Beam.batchSize batchSize,
      Se.Set Beam.bulkStatusCheckBatchLimit bulkStatusCheckBatchLimit,
      Se.Set Beam.bulkStatusCheckIntervalMinutes bulkStatusCheckIntervalMinutes,
      Se.Set Beam.counterpartyType counterpartyType,
      Se.Set Beam.dayOfMonth dayOfMonth,
      Se.Set Beam.dayOfWeek dayOfWeek,
      Se.Set Beam.defaultRailKey defaultRailKey,
      Se.Set Beam.frequency frequency,
      Se.Set Beam.intervalDays intervalDays,
      Se.Set Beam.intervalHours intervalHours,
      Se.Set Beam.isEnabled isEnabled,
      Se.Set Beam.maxRetries maxRetries,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.minimumAmount minimumAmount,
      Se.Set Beam.name name,
      Se.Set Beam.orderType orderType,
      Se.Set Beam.remark remark,
      Se.Set Beam.rescheduleBufferMinutes rescheduleBufferMinutes,
      Se.Set Beam.subLedger subLedger,
      Se.Set Beam.timeDiffFromUtc timeDiffFromUtc,
      Se.Set Beam.timeOfDay timeOfDay,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.ScheduledTransferRule Lib.Finance.Domain.Types.ScheduledTransferRule.ScheduledTransferRule where
  fromTType' (Beam.ScheduledTransferRuleT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.ScheduledTransferRule.ScheduledTransferRule
          { accountType = accountType,
            batchSize = batchSize,
            bulkStatusCheckBatchLimit = bulkStatusCheckBatchLimit,
            bulkStatusCheckIntervalMinutes = bulkStatusCheckIntervalMinutes,
            counterpartyType = counterpartyType,
            createdAt = createdAt,
            dayOfMonth = dayOfMonth,
            dayOfWeek = dayOfWeek,
            defaultRailKey = defaultRailKey,
            frequency = frequency,
            id = Kernel.Types.Id.Id id,
            intervalDays = intervalDays,
            intervalHours = intervalHours,
            isEnabled = isEnabled,
            maxRetries = maxRetries,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            minimumAmount = minimumAmount,
            name = name,
            orderType = orderType,
            remark = remark,
            rescheduleBufferMinutes = rescheduleBufferMinutes,
            subLedger = subLedger,
            timeDiffFromUtc = timeDiffFromUtc,
            timeOfDay = timeOfDay,
            updatedAt = updatedAt
          }

instance ToTType' Beam.ScheduledTransferRule Lib.Finance.Domain.Types.ScheduledTransferRule.ScheduledTransferRule where
  toTType' (Lib.Finance.Domain.Types.ScheduledTransferRule.ScheduledTransferRule {..}) = do
    Beam.ScheduledTransferRuleT
      { Beam.accountType = accountType,
        Beam.batchSize = batchSize,
        Beam.bulkStatusCheckBatchLimit = bulkStatusCheckBatchLimit,
        Beam.bulkStatusCheckIntervalMinutes = bulkStatusCheckIntervalMinutes,
        Beam.counterpartyType = counterpartyType,
        Beam.createdAt = createdAt,
        Beam.dayOfMonth = dayOfMonth,
        Beam.dayOfWeek = dayOfWeek,
        Beam.defaultRailKey = defaultRailKey,
        Beam.frequency = frequency,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.intervalDays = intervalDays,
        Beam.intervalHours = intervalHours,
        Beam.isEnabled = isEnabled,
        Beam.maxRetries = maxRetries,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.minimumAmount = minimumAmount,
        Beam.name = name,
        Beam.orderType = orderType,
        Beam.remark = remark,
        Beam.rescheduleBufferMinutes = rescheduleBufferMinutes,
        Beam.subLedger = subLedger,
        Beam.timeDiffFromUtc = timeDiffFromUtc,
        Beam.timeOfDay = timeOfDay,
        Beam.updatedAt = updatedAt
      }
