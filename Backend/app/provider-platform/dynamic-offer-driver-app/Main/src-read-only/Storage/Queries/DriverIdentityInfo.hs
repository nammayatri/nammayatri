{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverIdentityInfo where

import qualified Data.Aeson
import qualified Domain.Types.DriverIdentityInfo
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Verification.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Sequelize as Se
import qualified Storage.Beam.DriverIdentityInfo as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverIdentityInfo.DriverIdentityInfo -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverIdentityInfo.DriverIdentityInfo] -> m ())
createMany = traverse_ create

findByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverIdentityInfo.DriverIdentityInfo))
findByDriverId driverId = do findOneWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateCourtRecord ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.External.Verification.Types.CRCVerificationResponse -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateCourtRecord courtRecord driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.courtRecord (Data.Aeson.toJSON <$> courtRecord), Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverIdentityInfo.DriverIdentityInfo))
findByPrimaryKey driverId = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverIdentityInfo.DriverIdentityInfo -> m ())
updateByPrimaryKey (Domain.Types.DriverIdentityInfo.DriverIdentityInfo {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.address address,
      Se.Set Beam.addressDocumentType addressDocumentType,
      Se.Set Beam.addressState addressState,
      Se.Set Beam.courtRecord (Data.Aeson.toJSON <$> courtRecord),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.nomineeDob nomineeDob,
      Se.Set Beam.nomineeName nomineeName,
      Se.Set Beam.nomineeRelationship nomineeRelationship,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]

instance FromTType' Beam.DriverIdentityInfo Domain.Types.DriverIdentityInfo.DriverIdentityInfo where
  fromTType' (Beam.DriverIdentityInfoT {..}) = do
    pure $
      Just
        Domain.Types.DriverIdentityInfo.DriverIdentityInfo
          { address = address,
            addressDocumentType = addressDocumentType,
            addressState = addressState,
            courtRecord = Kernel.Utils.JSON.valueToMaybe =<< courtRecord,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            nomineeDob = nomineeDob,
            nomineeName = nomineeName,
            nomineeRelationship = nomineeRelationship,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverIdentityInfo Domain.Types.DriverIdentityInfo.DriverIdentityInfo where
  toTType' (Domain.Types.DriverIdentityInfo.DriverIdentityInfo {..}) = do
    Beam.DriverIdentityInfoT
      { Beam.address = address,
        Beam.addressDocumentType = addressDocumentType,
        Beam.addressState = addressState,
        Beam.courtRecord = Data.Aeson.toJSON <$> courtRecord,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.nomineeDob = nomineeDob,
        Beam.nomineeName = nomineeName,
        Beam.nomineeRelationship = nomineeRelationship,
        Beam.updatedAt = updatedAt
      }
