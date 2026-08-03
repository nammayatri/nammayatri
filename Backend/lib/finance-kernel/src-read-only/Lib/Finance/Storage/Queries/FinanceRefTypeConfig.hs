{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.FinanceRefTypeConfig (module Lib.Finance.Storage.Queries.FinanceRefTypeConfig, module ReExport) where

import qualified Data.Aeson
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.FinanceRefTypeConfig
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.FinanceRefTypeConfig as Beam
import Lib.Finance.Storage.Queries.FinanceRefTypeConfigExtra as ReExport
import qualified Lib.Finance.Types.ChargeValue
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.FinanceRefTypeConfig.FinanceRefTypeConfig -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.FinanceRefTypeConfig.FinanceRefTypeConfig] -> m ())
createMany = traverse_ create

findAllByOpCity :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.FinanceRefTypeConfig.FinanceRefTypeConfig]))
findAllByOpCity merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.FinanceRefTypeConfig.FinanceRefTypeConfig -> m (Maybe Lib.Finance.Domain.Types.FinanceRefTypeConfig.FinanceRefTypeConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.FinanceRefTypeConfig.FinanceRefTypeConfig -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.FinanceRefTypeConfig.FinanceRefTypeConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.commissionValueAmount (Lib.Finance.Types.ChargeValue.chargeValueAmount <$> commissionValue),
      Se.Set Beam.commissionValueType (Lib.Finance.Types.ChargeValue.chargeValueType <$> commissionValue),
      Se.Set Beam.directTaxRates (Data.Aeson.toJSON <$> directTaxRates),
      Se.Set Beam.enabled enabled,
      Se.Set Beam.indirectTaxDirection indirectTaxDirection,
      Se.Set Beam.isTaxExclusive isTaxExclusive,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.referenceType referenceType,
      Se.Set Beam.taxRateType (Lib.Finance.Types.ChargeValue.chargeValueType <$> taxRate),
      Se.Set Beam.taxRateValue (Lib.Finance.Types.ChargeValue.chargeValueAmount <$> taxRate),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
