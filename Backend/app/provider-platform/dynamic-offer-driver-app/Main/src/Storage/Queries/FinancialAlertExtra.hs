{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FinancialAlertExtra where

import qualified Domain.Types.FinancialAlert
import qualified Domain.Types.Merchant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FinancialAlert as Beam
import Storage.Queries.OrphanInstances.FinancialAlert

findAllByMerchantIdWithLimitOffset ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Maybe Domain.Types.FinancialAlert.AlertStatus ->
  Maybe Domain.Types.FinancialAlert.AlertSeverity ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.Types.FinancialAlert.FinancialAlert]
findAllByMerchantIdWithLimitOffset merchantId mbStatus mbSeverity mbLimit mbOffset = do
  let limitVal = min 50 $ fromMaybe 10 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]
          <> maybe [] (\s -> [Se.Is Beam.status $ Se.Eq s]) mbStatus
          <> maybe [] (\s -> [Se.Is Beam.severity $ Se.Eq s]) mbSeverity
    ]
    (Se.Desc Beam.createdAt)
    (Just limitVal)
    (Just offsetVal)

findOpenAlertsByMerchantIdAndSeverity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Domain.Types.FinancialAlert.AlertSeverity ->
  m [Domain.Types.FinancialAlert.FinancialAlert]
findOpenAlertsByMerchantIdAndSeverity merchantId severity = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.status $ Se.Eq Domain.Types.FinancialAlert.OPEN,
          Se.Is Beam.severity $ Se.Eq severity
        ]
    ]
