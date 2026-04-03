{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.MorthVerification where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.MorthVerification
import qualified Storage.Beam.MorthVerification as Beam
import qualified Domain.Types.DocumentVerificationConfig
import qualified Kernel.External.Encryption
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MorthVerification.MorthVerification -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MorthVerification.MorthVerification] -> m ())
createMany = traverse_ create
deleteByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByPersonId driverId = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
findAllByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.MorthVerification.MorthVerification]))
findAllByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
findAllByDriverIdAndDocType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                               (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.DocumentVerificationConfig.DocumentType -> m ([Domain.Types.MorthVerification.MorthVerification]))
findAllByDriverIdAndDocType driverId docType = do findAllWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.docType $ Se.Eq docType]]
findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MorthVerification.MorthVerification -> m (Maybe Domain.Types.MorthVerification.MorthVerification))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
findByRequestId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.MorthVerification.MorthVerification))
findByRequestId requestId = do findOneWithKV [Se.Is Beam.requestId $ Se.Eq requestId]
findLatestByDriverIdAndDocTypeAndDocumentNumber :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                                   (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.DocumentVerificationConfig.DocumentType -> Kernel.External.Encryption.DbHash -> m ([Domain.Types.MorthVerification.MorthVerification]))
findLatestByDriverIdAndDocTypeAndDocumentNumber limit offset driverId docType documentNumberHashBeam = do findAllWithOptionsKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
                                                                                                                                        Se.Is Beam.docType $ Se.Eq docType,
                                                                                                                                        Se.Is Beam.documentNumberHash $ Se.Eq documentNumberHashBeam]] (Se.Desc Beam.createdAt) limit offset
updateMerchantIdAndCityIdByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                       (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateMerchantIdAndCityIdByDriverId merchantId merchantOperatingCityId driverId = do {_now <- getCurrentTime;
                                                                                      updateWithKV [Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
                                                                                                    Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
                                                                                                    Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]}
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MorthVerification.MorthVerification -> m (Maybe Domain.Types.MorthVerification.MorthVerification))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MorthVerification.MorthVerification -> m ())
updateByPrimaryKey (Domain.Types.MorthVerification.MorthVerification {..}) = do {_now <- getCurrentTime;
                                                                                 updateWithKV [Se.Set Beam.docType docType,
                                                                                               Se.Set Beam.documentNumberEncrypted (((documentNumber <&> unEncrypted . (.encrypted)))),
                                                                                               Se.Set Beam.documentNumberHash ((documentNumber <&> (.hash))),
                                                                                               Se.Set Beam.driverDateOfBirth driverDateOfBirth,
                                                                                               Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
                                                                                               Se.Set Beam.issueDateOnDoc issueDateOnDoc,
                                                                                               Se.Set Beam.message message,
                                                                                               Se.Set Beam.morthResponse morthResponse,
                                                                                               Se.Set Beam.requestId requestId,
                                                                                               Se.Set Beam.status status,
                                                                                               Se.Set Beam.statusCode statusCode,
                                                                                               Se.Set Beam.vehicleCategory vehicleCategory,
                                                                                               Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
                                                                                               Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
                                                                                               Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



instance FromTType' Beam.MorthVerification Domain.Types.MorthVerification.MorthVerification
    where fromTType' (Beam.MorthVerificationT {..}) = do pure $ Just Domain.Types.MorthVerification.MorthVerification{docType = docType,
                                                                                                                      documentNumber = EncryptedHashed <$> (Encrypted <$> documentNumberEncrypted) <*> documentNumberHash,
                                                                                                                      driverDateOfBirth = driverDateOfBirth,
                                                                                                                      driverId = Kernel.Types.Id.Id driverId,
                                                                                                                      id = Kernel.Types.Id.Id id,
                                                                                                                      issueDateOnDoc = issueDateOnDoc,
                                                                                                                      message = message,
                                                                                                                      morthResponse = morthResponse,
                                                                                                                      requestId = requestId,
                                                                                                                      status = status,
                                                                                                                      statusCode = statusCode,
                                                                                                                      vehicleCategory = vehicleCategory,
                                                                                                                      merchantId = Kernel.Types.Id.Id <$> merchantId,
                                                                                                                      merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
                                                                                                                      createdAt = createdAt,
                                                                                                                      updatedAt = updatedAt}
instance ToTType' Beam.MorthVerification Domain.Types.MorthVerification.MorthVerification
    where toTType' (Domain.Types.MorthVerification.MorthVerification {..}) = do Beam.MorthVerificationT{Beam.docType = docType,
                                                                                                        Beam.documentNumberEncrypted = ((documentNumber <&> unEncrypted . (.encrypted))),
                                                                                                        Beam.documentNumberHash = (documentNumber <&> (.hash)),
                                                                                                        Beam.driverDateOfBirth = driverDateOfBirth,
                                                                                                        Beam.driverId = Kernel.Types.Id.getId driverId,
                                                                                                        Beam.id = Kernel.Types.Id.getId id,
                                                                                                        Beam.issueDateOnDoc = issueDateOnDoc,
                                                                                                        Beam.message = message,
                                                                                                        Beam.morthResponse = morthResponse,
                                                                                                        Beam.requestId = requestId,
                                                                                                        Beam.status = status,
                                                                                                        Beam.statusCode = statusCode,
                                                                                                        Beam.vehicleCategory = vehicleCategory,
                                                                                                        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
                                                                                                        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
                                                                                                        Beam.createdAt = createdAt,
                                                                                                        Beam.updatedAt = updatedAt}



