{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.StopFare where

import qualified Domain.Types.FRFSFarePolicy
import qualified Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.StopFare
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.StopFare as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.StopFare.StopFare -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.StopFare.StopFare] -> m ())
createMany = traverse_ create

findAllByStartStopAndIntegratedBPPConfigId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m [Domain.Types.StopFare.StopFare])
findAllByStartStopAndIntegratedBPPConfigId startStopCode endStopCode integratedBppConfigId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.startStopCode $ Se.Eq startStopCode,
          Se.Is Beam.endStopCode $ Se.Eq endStopCode,
          Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId)
        ]
    ]

findByRouteCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSFarePolicy.FRFSFarePolicy -> m [Domain.Types.StopFare.StopFare])
findByRouteCode farePolicyId = do findAllWithKV [Se.And [Se.Is Beam.farePolicyId $ Se.Eq (Kernel.Types.Id.getId farePolicyId)]]

findByRouteStartAndStopCode ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSFarePolicy.FRFSFarePolicy -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Domain.Types.StopFare.StopFare))
findByRouteStartAndStopCode farePolicyId startStopCode endStopCode = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.farePolicyId $ Se.Eq (Kernel.Types.Id.getId farePolicyId),
          Se.Is Beam.startStopCode $ Se.Eq startStopCode,
          Se.Is Beam.endStopCode $ Se.Eq endStopCode
        ]
    ]

findByStartAndEndStopCodeAndIntegratedBPPConfigId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m (Maybe Domain.Types.StopFare.StopFare))
findByStartAndEndStopCodeAndIntegratedBPPConfigId startStopCode endStopCode integratedBppConfigId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.startStopCode $ Se.Eq startStopCode,
          Se.Is Beam.endStopCode $ Se.Eq endStopCode,
          Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId)
        ]
    ]

updateFareByStopCodes ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.FRFSFarePolicy.FRFSFarePolicy -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Domain.Types.FRFSQuoteCategoryType.FRFSQuoteCategoryType -> m ())
updateFareByStopCodes amount farePolicyId startStopCode endStopCode category = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.amount amount, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.farePolicyId $ Se.Eq (Kernel.Types.Id.getId farePolicyId),
          Se.Is Beam.startStopCode $ Se.Eq startStopCode,
          Se.Is Beam.endStopCode $ Se.Eq endStopCode,
          Se.Is Beam.category $ Se.Eq (Kernel.Prelude.Just category)
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.FRFSQuoteCategoryType.FRFSQuoteCategoryType -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.FRFSFarePolicy.FRFSFarePolicy -> Kernel.Prelude.Text -> m (Maybe Domain.Types.StopFare.StopFare))
findByPrimaryKey category endStopCode farePolicyId startStopCode = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.category $ Se.Eq (Kernel.Prelude.Just category),
          Se.Is Beam.endStopCode $ Se.Eq endStopCode,
          Se.Is Beam.farePolicyId $ Se.Eq (Kernel.Types.Id.getId farePolicyId),
          Se.Is Beam.startStopCode $ Se.Eq startStopCode
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.StopFare.StopFare -> m ())
updateByPrimaryKey (Domain.Types.StopFare.StopFare {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.bppItemId bppItemId,
      Se.Set Beam.currency currency,
      Se.Set Beam.integratedBppConfigId (Kernel.Types.Id.getId integratedBppConfigId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.offeredAmount offeredAmount,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.category $ Se.Eq (Kernel.Prelude.Just category),
          Se.Is Beam.endStopCode $ Se.Eq endStopCode,
          Se.Is Beam.farePolicyId $ Se.Eq (Kernel.Types.Id.getId farePolicyId),
          Se.Is Beam.startStopCode $ Se.Eq startStopCode
        ]
    ]

instance FromTType' Beam.StopFare Domain.Types.StopFare.StopFare where
  fromTType' (Beam.StopFareT {..}) = do
    pure $
      Just
        Domain.Types.StopFare.StopFare
          { amount = amount,
            bppItemId = bppItemId,
            category = Kernel.Prelude.fromMaybe Domain.Types.FRFSQuoteCategoryType.ADULT category,
            currency = currency,
            endStopCode = endStopCode,
            farePolicyId = Kernel.Types.Id.Id farePolicyId,
            integratedBppConfigId = Kernel.Types.Id.Id integratedBppConfigId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            offeredAmount = offeredAmount,
            startStopCode = startStopCode,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.StopFare Domain.Types.StopFare.StopFare where
  toTType' (Domain.Types.StopFare.StopFare {..}) = do
    Beam.StopFareT
      { Beam.amount = amount,
        Beam.bppItemId = bppItemId,
        Beam.category = Kernel.Prelude.Just category,
        Beam.currency = currency,
        Beam.endStopCode = endStopCode,
        Beam.farePolicyId = Kernel.Types.Id.getId farePolicyId,
        Beam.integratedBppConfigId = Kernel.Types.Id.getId integratedBppConfigId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.offeredAmount = offeredAmount,
        Beam.startStopCode = startStopCode,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
