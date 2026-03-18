{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.Chargeback (module Lib.Finance.Storage.Queries.Chargeback, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.Chargeback
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.Chargeback as Beam
import Lib.Finance.Storage.Queries.ChargebackExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.Chargeback.Chargeback -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.Chargeback.Chargeback] -> m ())
createMany = traverse_ create

findById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.Chargeback.Chargeback -> m (Maybe Lib.Finance.Domain.Types.Chargeback.Chargeback))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findBySettlementReportId ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.PgPaymentSettlementReport.PgPaymentSettlementReport -> m [Lib.Finance.Domain.Types.Chargeback.Chargeback])
findBySettlementReportId settlementReportId = do findAllWithKV [Se.Is Beam.settlementReportId $ Se.Eq (Kernel.Types.Id.getId settlementReportId)]

findByTransactionId ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> m [Lib.Finance.Domain.Types.Chargeback.Chargeback])
findByTransactionId transactionId = do findAllWithKV [Se.Is Beam.transactionId $ Se.Eq transactionId]

updateChargebackStatus ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Finance.Domain.Types.Chargeback.ChargebackStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.Chargeback.Chargeback -> m ())
updateChargebackStatus chargebackStatus evidenceUrl adminNotes id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.chargebackStatus chargebackStatus,
      Se.Set Beam.evidenceUrl evidenceUrl,
      Se.Set Beam.adminNotes adminNotes,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.Chargeback.Chargeback -> m (Maybe Lib.Finance.Domain.Types.Chargeback.Chargeback))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.Chargeback.Chargeback -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.Chargeback.Chargeback {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.settlementReportId (Kernel.Types.Id.getId settlementReportId),
      Se.Set Beam.transactionId transactionId,
      Se.Set Beam.chargebackReasonCode chargebackReasonCode,
      Se.Set Beam.chargebackAmount chargebackAmount,
      Se.Set Beam.chargebackStatus chargebackStatus,
      Se.Set Beam.responseDeadline responseDeadline,
      Se.Set Beam.evidenceUrl evidenceUrl,
      Se.Set Beam.adminNotes adminNotes,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
