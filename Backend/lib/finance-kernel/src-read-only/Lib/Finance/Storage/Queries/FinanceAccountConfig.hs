{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.FinanceAccountConfig where

import qualified Data.Aeson
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Lib.Finance.Domain.Types.FinanceAccountConfig
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.FinanceAccountConfig as Beam
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.FinanceAccountConfig.FinanceAccountConfig -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.FinanceAccountConfig.FinanceAccountConfig] -> m ())
createMany = traverse_ create

findAllByMerchantId :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m [Lib.Finance.Domain.Types.FinanceAccountConfig.FinanceAccountConfig])
findAllByMerchantId merchantId = do findAllWithKV [Se.Is Beam.merchantId $ Se.Eq merchantId]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.FinanceAccountConfig.FinanceAccountConfig -> m (Maybe Lib.Finance.Domain.Types.FinanceAccountConfig.FinanceAccountConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.FinanceAccountConfig.FinanceAccountConfig -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.FinanceAccountConfig.FinanceAccountConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.accountType accountType,
      Se.Set Beam.allowNegativeBalance allowNegativeBalance,
      Se.Set Beam.autoTransferOut autoTransferOut,
      Se.Set Beam.counterpartyType counterpartyType,
      Se.Set Beam.instantTransferOutEnabled instantTransferOutEnabled,
      Se.Set Beam.maxTransferOutPerDay maxTransferOutPerDay,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.minRetainedBalance minRetainedBalance,
      Se.Set Beam.minTransferOutAmount minTransferOutAmount,
      Se.Set Beam.pointsConversionRate pointsConversionRate,
      Se.Set Beam.redeemableCutoff (Data.Aeson.toJSON <$> redeemableCutoff),
      Se.Set Beam.statusCheckIntervalSec statusCheckIntervalSec,
      Se.Set Beam.statusCheckMaxAttempts statusCheckMaxAttempts,
      Se.Set Beam.subLedger subLedger,
      Se.Set Beam.transferInEnabled transferInEnabled,
      Se.Set Beam.transferOutEnabled transferOutEnabled,
      Se.Set Beam.transferOutFee (Data.Aeson.toJSON <$> transferOutFee),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FinanceAccountConfig Lib.Finance.Domain.Types.FinanceAccountConfig.FinanceAccountConfig where
  fromTType' (Beam.FinanceAccountConfigT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.FinanceAccountConfig.FinanceAccountConfig
          { accountType = accountType,
            allowNegativeBalance = allowNegativeBalance,
            autoTransferOut = autoTransferOut,
            counterpartyType = counterpartyType,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            instantTransferOutEnabled = instantTransferOutEnabled,
            maxTransferOutPerDay = maxTransferOutPerDay,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            minRetainedBalance = minRetainedBalance,
            minTransferOutAmount = minTransferOutAmount,
            pointsConversionRate = pointsConversionRate,
            redeemableCutoff = redeemableCutoff >>= Kernel.Utils.JSON.valueToMaybe,
            statusCheckIntervalSec = statusCheckIntervalSec,
            statusCheckMaxAttempts = statusCheckMaxAttempts,
            subLedger = subLedger,
            transferInEnabled = transferInEnabled,
            transferOutEnabled = transferOutEnabled,
            transferOutFee = transferOutFee >>= Kernel.Utils.JSON.valueToMaybe,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FinanceAccountConfig Lib.Finance.Domain.Types.FinanceAccountConfig.FinanceAccountConfig where
  toTType' (Lib.Finance.Domain.Types.FinanceAccountConfig.FinanceAccountConfig {..}) = do
    Beam.FinanceAccountConfigT
      { Beam.accountType = accountType,
        Beam.allowNegativeBalance = allowNegativeBalance,
        Beam.autoTransferOut = autoTransferOut,
        Beam.counterpartyType = counterpartyType,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.instantTransferOutEnabled = instantTransferOutEnabled,
        Beam.maxTransferOutPerDay = maxTransferOutPerDay,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.minRetainedBalance = minRetainedBalance,
        Beam.minTransferOutAmount = minTransferOutAmount,
        Beam.pointsConversionRate = pointsConversionRate,
        Beam.redeemableCutoff = Data.Aeson.toJSON <$> redeemableCutoff,
        Beam.statusCheckIntervalSec = statusCheckIntervalSec,
        Beam.statusCheckMaxAttempts = statusCheckMaxAttempts,
        Beam.subLedger = subLedger,
        Beam.transferInEnabled = transferInEnabled,
        Beam.transferOutEnabled = transferOutEnabled,
        Beam.transferOutFee = Data.Aeson.toJSON <$> transferOutFee,
        Beam.updatedAt = updatedAt
      }
