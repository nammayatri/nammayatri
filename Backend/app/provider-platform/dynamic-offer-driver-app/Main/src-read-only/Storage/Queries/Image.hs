{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Image (module Storage.Queries.Image, module ReExport) where

import qualified Domain.Types.Image
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Image as Beam
import Storage.Queries.ImageExtra as ReExport
import qualified Tools.Error

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Image.Image -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Image.Image] -> m ())
createMany = traverse_ create

addFailureReason :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Tools.Error.DriverOnboardingError -> Kernel.Types.Id.Id Domain.Types.Image.Image -> m ())
addFailureReason failureReason (Kernel.Types.Id.Id id) = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.failureReason failureReason] [Se.Is Beam.id $ Se.Eq id]

deleteByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByPersonId (Kernel.Types.Id.Id personId) = do deleteWithKV [Se.Is Beam.personId $ Se.Eq personId]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Image.Image -> m (Maybe Domain.Types.Image.Image))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByMerchantId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m ([Domain.Types.Image.Image]))
findByMerchantId (Kernel.Types.Id.Id merchantId) = do findAllWithKV [Se.Is Beam.merchantId $ Se.Eq merchantId]

findImagesByPersonAndType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.Image.ImageType -> m ([Domain.Types.Image.Image]))
findImagesByPersonAndType (Kernel.Types.Id.Id merchantId) (Kernel.Types.Id.Id personId) imageType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.personId $ Se.Eq personId,
          Se.Is Beam.imageType $ Se.Eq imageType
        ]
    ]

updateToValid :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Image.Image -> m ())
updateToValid isValid (Kernel.Types.Id.Id id) = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.isValid isValid, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Image.Image -> m (Maybe Domain.Types.Image.Image))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Image.Image -> m ())
updateByPrimaryKey (Domain.Types.Image.Image {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.failureReason failureReason,
      Se.Set Beam.imageType imageType,
      Se.Set Beam.isValid isValid,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.s3Path s3Path,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
