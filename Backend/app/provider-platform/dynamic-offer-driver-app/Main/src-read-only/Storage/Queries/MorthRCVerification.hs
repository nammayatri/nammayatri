{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MorthRCVerification where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MorthRCVerification
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MorthRCVerification as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MorthRCVerification.MorthRCVerification -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MorthRCVerification.MorthRCVerification] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MorthRCVerification.MorthRCVerification -> m (Maybe Domain.Types.MorthRCVerification.MorthRCVerification))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findAllByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.MorthRCVerification.MorthRCVerification])
findAllByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findLatestByDriverIdAndRCNumber ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> m [Domain.Types.MorthRCVerification.MorthRCVerification])
findLatestByDriverIdAndRCNumber limit offset driverId rcNumber = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.rcNumber $ Se.Eq rcNumber
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

deleteByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByPersonId driverId = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MorthRCVerification.MorthRCVerification -> m (Maybe Domain.Types.MorthRCVerification.MorthRCVerification))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.MorthRCVerification Domain.Types.MorthRCVerification.MorthRCVerification where
  fromTType' (Beam.MorthRCVerificationT {..}) = do
    pure $
      Just
        Domain.Types.MorthRCVerification.MorthRCVerification
          { id = Kernel.Types.Id.Id id,
            driverId = Kernel.Types.Id.Id driverId,
            rcNumber = rcNumber,
            success = success,
            rcValidity = rcValidity,
            rcValidUpto = rcValidUpto,
            message = message,
            statusCode = statusCode,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MorthRCVerification Domain.Types.MorthRCVerification.MorthRCVerification where
  toTType' (Domain.Types.MorthRCVerification.MorthRCVerification {..}) = do
    Beam.MorthRCVerificationT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.rcNumber = rcNumber,
        Beam.success = success,
        Beam.rcValidity = rcValidity,
        Beam.rcValidUpto = rcValidUpto,
        Beam.message = message,
        Beam.statusCode = statusCode,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
