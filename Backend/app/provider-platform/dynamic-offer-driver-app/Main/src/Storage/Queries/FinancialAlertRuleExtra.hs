{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FinancialAlertRuleExtra where

import qualified Domain.Types.FinancialAlertRule
import qualified Domain.Types.Merchant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FinancialAlertRule as Beam
import Storage.Queries.OrphanInstances.FinancialAlertRule

findAllByMerchantIdWithLimitOffset ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Maybe Domain.Types.FinancialAlertRule.AlertCategory ->
  Maybe Bool ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.Types.FinancialAlertRule.FinancialAlertRule]
findAllByMerchantIdWithLimitOffset merchantId mbCategory mbIsActive mbLimit mbOffset = do
  let limitVal = min 50 $ fromMaybe 10 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]
          <> maybe [] (\c -> [Se.Is Beam.category $ Se.Eq c]) mbCategory
          <> maybe [] (\a -> [Se.Is Beam.isActive $ Se.Eq a]) mbIsActive
    ]
    (Se.Desc Beam.createdAt)
    (Just limitVal)
    (Just offsetVal)

updateFullById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Domain.Types.FinancialAlertRule.FinancialAlertRule ->
  m ()
updateFullById rule = do
  now <- getCurrentTime
  updateOneWithKV
    ( [ Se.Set Beam.name rule.name,
        Se.Set Beam.alertType rule.alertType,
        Se.Set Beam.category rule.category,
        Se.Set Beam.conditionJson rule.conditionJson,
        Se.Set Beam.severity rule.severity,
        Se.Set Beam.channels rule.channels,
        Se.Set Beam.cooldownMinutes rule.cooldownMinutes,
        Se.Set Beam.escalationMinutes rule.escalationMinutes,
        Se.Set Beam.isActive rule.isActive,
        Se.Set Beam.updatedAt now
      ]
    )
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId rule.id)]
