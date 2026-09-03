{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.PgSettlementBatch where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.PgSettlementBatch
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.PgSettlementBatch as Beam
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.PgSettlementBatch.PgSettlementBatch -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.PgSettlementBatch.PgSettlementBatch] -> m ())
createMany = traverse_ create

findAllByMerchantCityAndGateway ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.PgSettlementBatch.PgSettlementBatch]))
findAllByMerchantCityAndGateway merchantId merchantOperatingCityId paymentGateway = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.paymentGateway $ Se.Eq paymentGateway
        ]
    ]

findById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.PgSettlementBatch.PgSettlementBatch -> m (Maybe Lib.Finance.Domain.Types.PgSettlementBatch.PgSettlementBatch))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByMerchantCityGatewayAndPvNumber ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Lib.Finance.Domain.Types.PgSettlementBatch.PgSettlementBatch))
findByMerchantCityGatewayAndPvNumber merchantId merchantOperatingCityId paymentGateway pvNumber = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.paymentGateway $ Se.Eq paymentGateway,
          Se.Is Beam.pvNumber $ Se.Eq pvNumber
        ]
    ]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.PgSettlementBatch.PgSettlementBatch -> m (Maybe Lib.Finance.Domain.Types.PgSettlementBatch.PgSettlementBatch))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.PgSettlementBatch.PgSettlementBatch -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.PgSettlementBatch.PgSettlementBatch {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.adjustmentAmount adjustmentAmount,
      Se.Set Beam.chargebackAmount chargebackAmount,
      Se.Set Beam.chargebackReversalAmount chargebackReversalAmount,
      Se.Set Beam.charges charges,
      Se.Set Beam.currency currency,
      Se.Set Beam.mercId mercId,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.objectId objectId,
      Se.Set Beam.otherAdjustments otherAdjustments,
      Se.Set Beam.paymentGateway paymentGateway,
      Se.Set Beam.payoutAmount payoutAmount,
      Se.Set Beam.payoutMercId payoutMercId,
      Se.Set Beam.pvFile pvFile,
      Se.Set Beam.pvFileDate pvFileDate,
      Se.Set Beam.pvNumber pvNumber,
      Se.Set Beam.refundAmount refundAmount,
      Se.Set Beam.refundReversalAmount refundReversalAmount,
      Se.Set Beam.settlementAmount settlementAmount,
      Se.Set Beam.settlementDate settlementDate,
      Se.Set Beam.status status,
      Se.Set Beam.taxes taxes,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.utr utr,
      Se.Set Beam.utrDate utrDate
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PgSettlementBatch Lib.Finance.Domain.Types.PgSettlementBatch.PgSettlementBatch where
  fromTType' (Beam.PgSettlementBatchT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.PgSettlementBatch.PgSettlementBatch
          { adjustmentAmount = adjustmentAmount,
            chargebackAmount = chargebackAmount,
            chargebackReversalAmount = chargebackReversalAmount,
            charges = charges,
            createdAt = createdAt,
            currency = currency,
            id = Kernel.Types.Id.Id id,
            mercId = mercId,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            objectId = objectId,
            otherAdjustments = otherAdjustments,
            paymentGateway = paymentGateway,
            payoutAmount = payoutAmount,
            payoutMercId = payoutMercId,
            pvFile = pvFile,
            pvFileDate = pvFileDate,
            pvNumber = pvNumber,
            refundAmount = refundAmount,
            refundReversalAmount = refundReversalAmount,
            settlementAmount = settlementAmount,
            settlementDate = settlementDate,
            status = status,
            taxes = taxes,
            updatedAt = updatedAt,
            utr = utr,
            utrDate = utrDate
          }

instance ToTType' Beam.PgSettlementBatch Lib.Finance.Domain.Types.PgSettlementBatch.PgSettlementBatch where
  toTType' (Lib.Finance.Domain.Types.PgSettlementBatch.PgSettlementBatch {..}) = do
    Beam.PgSettlementBatchT
      { Beam.adjustmentAmount = adjustmentAmount,
        Beam.chargebackAmount = chargebackAmount,
        Beam.chargebackReversalAmount = chargebackReversalAmount,
        Beam.charges = charges,
        Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.mercId = mercId,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.objectId = objectId,
        Beam.otherAdjustments = otherAdjustments,
        Beam.paymentGateway = paymentGateway,
        Beam.payoutAmount = payoutAmount,
        Beam.payoutMercId = payoutMercId,
        Beam.pvFile = pvFile,
        Beam.pvFileDate = pvFileDate,
        Beam.pvNumber = pvNumber,
        Beam.refundAmount = refundAmount,
        Beam.refundReversalAmount = refundReversalAmount,
        Beam.settlementAmount = settlementAmount,
        Beam.settlementDate = settlementDate,
        Beam.status = status,
        Beam.taxes = taxes,
        Beam.updatedAt = updatedAt,
        Beam.utr = utr,
        Beam.utrDate = utrDate
      }
