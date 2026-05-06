{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.SettlementFileInfo where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.SettlementFileInfo
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.SettlementFileInfo as Beam
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileInfo -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileInfo] -> m ())
createMany = traverse_ create

findAllByPaymentGatewayMerchantCityAndStatus ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileStatus -> m ([Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileInfo]))
findAllByPaymentGatewayMerchantCityAndStatus paymentGatewayName merchantId merchantOperatingCityId status = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.paymentGatewayName $ Se.Eq paymentGatewayName,
          Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.status $ Se.Eq status
        ]
    ]

findById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileInfo -> m (Maybe Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileInfo))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByMerchantCityGatewayAndFileName ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileInfo))
findByMerchantCityGatewayAndFileName merchantId merchantOperatingCityId paymentGatewayName fileName = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.paymentGatewayName $ Se.Eq paymentGatewayName,
          Se.Is Beam.fileName $ Se.Eq fileName
        ]
    ]

findByPaymentGatewayNameAndFileName ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileInfo))
findByPaymentGatewayNameAndFileName paymentGatewayName fileName = do findOneWithKV [Se.And [Se.Is Beam.paymentGatewayName $ Se.Eq paymentGatewayName, Se.Is Beam.fileName $ Se.Eq fileName]]

updateProgress ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileStatus -> Kernel.Prelude.Int -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileInfo -> m ())
updateProgress status lastProcessedIndex id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.status status, Se.Set Beam.lastProcessedIndex lastProcessedIndex, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileStatus -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileInfo -> m ())
updateStatus status id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileInfo -> m (Maybe Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileInfo))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileInfo -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileInfo {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.fileName fileName,
      Se.Set Beam.lastProcessedIndex lastProcessedIndex,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.paymentGatewayName paymentGatewayName,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SettlementFileInfo Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileInfo where
  fromTType' (Beam.SettlementFileInfoT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileInfo
          { createdAt = createdAt,
            fileName = fileName,
            id = Kernel.Types.Id.Id id,
            lastProcessedIndex = lastProcessedIndex,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            paymentGatewayName = paymentGatewayName,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SettlementFileInfo Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileInfo where
  toTType' (Lib.Finance.Domain.Types.SettlementFileInfo.SettlementFileInfo {..}) = do
    Beam.SettlementFileInfoT
      { Beam.createdAt = createdAt,
        Beam.fileName = fileName,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lastProcessedIndex = lastProcessedIndex,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.paymentGatewayName = paymentGatewayName,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
