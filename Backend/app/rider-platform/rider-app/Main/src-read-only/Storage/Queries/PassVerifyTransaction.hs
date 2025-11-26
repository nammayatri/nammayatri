{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PassVerifyTransaction (module Storage.Queries.PassVerifyTransaction, module ReExport) where

import qualified Domain.Types.PassVerifyTransaction
import qualified Domain.Types.PurchasedPass
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PassVerifyTransaction as Beam
import Storage.Queries.PassVerifyTransactionExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PassVerifyTransaction.PassVerifyTransaction -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PassVerifyTransaction.PassVerifyTransaction] -> m ())
createMany = traverse_ create

findAllByPurchasePassId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> m ([Domain.Types.PassVerifyTransaction.PassVerifyTransaction]))
findAllByPurchasePassId purchasePassId = do findAllWithKV [Se.Is Beam.purchasePassId $ Se.Eq (Kernel.Types.Id.getId purchasePassId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.PassVerifyTransaction.PassVerifyTransaction -> m (Maybe Domain.Types.PassVerifyTransaction.PassVerifyTransaction))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PassVerifyTransaction.PassVerifyTransaction -> m ())
updateByPrimaryKey (Domain.Types.PassVerifyTransaction.PassVerifyTransaction {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.destinationStopCode destinationStopCode,
      Se.Set Beam.fleetId fleetId,
      Se.Set Beam.purchasePassId (Kernel.Types.Id.getId purchasePassId),
      Se.Set Beam.sourceStopCode sourceStopCode,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.verifiedAt verifiedAt,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
