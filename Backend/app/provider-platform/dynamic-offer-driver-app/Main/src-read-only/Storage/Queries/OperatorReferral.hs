{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OperatorReferral (module Storage.Queries.OperatorReferral, module ReExport) where

import qualified Data.Text
import qualified Domain.Types.OperatorReferral
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.OperatorReferral as Beam
import Storage.Queries.OperatorReferralExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.OperatorReferral.OperatorReferral -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.OperatorReferral.OperatorReferral] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> m (Maybe Domain.Types.OperatorReferral.OperatorReferral))
findById operatorId = do findOneWithKV [Se.Is Beam.operatorId $ Se.Eq operatorId]

findByReferralCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.OperatorReferral.OperatorReferral -> m (Maybe Domain.Types.OperatorReferral.OperatorReferral))
findByReferralCode referralCode = do findOneWithKV [Se.Is Beam.referralCode $ Se.Eq (Kernel.Types.Id.getId referralCode)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.OperatorReferral.OperatorReferral -> m (Maybe Domain.Types.OperatorReferral.OperatorReferral))
findByPrimaryKey referralCode = do findOneWithKV [Se.And [Se.Is Beam.referralCode $ Se.Eq (Kernel.Types.Id.getId referralCode)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.OperatorReferral.OperatorReferral -> m ())
updateByPrimaryKey (Domain.Types.OperatorReferral.OperatorReferral {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.linkedAt linkedAt,
      Se.Set Beam.operatorId operatorId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.referralCode $ Se.Eq (Kernel.Types.Id.getId referralCode)]]
