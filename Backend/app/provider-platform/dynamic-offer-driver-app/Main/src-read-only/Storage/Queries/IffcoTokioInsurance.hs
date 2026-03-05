{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.IffcoTokioInsurance (module Storage.Queries.IffcoTokioInsurance, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.IffcoTokioInsuranceExtra as ReExport
import qualified Domain.Types.IffcoTokioInsurance
import qualified Storage.Beam.IffcoTokioInsurance as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IffcoTokioInsurance.IffcoTokioInsurance -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.IffcoTokioInsurance.IffcoTokioInsurance] -> m ())
createMany = traverse_ create
findAllByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.IffcoTokioInsurance.IffcoTokioInsurance]))
findAllByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
findByInvoiceRequestNumber :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.IffcoTokioInsurance.IffcoTokioInsurance))
findByInvoiceRequestNumber invoiceRequestNumber = do findOneWithKV [Se.Is Beam.invoiceRequestNumber $ Se.Eq invoiceRequestNumber]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Kernel.Types.Id.Id Domain.Types.IffcoTokioInsurance.IffcoTokioInsurance -> m (Maybe Domain.Types.IffcoTokioInsurance.IffcoTokioInsurance))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IffcoTokioInsurance.IffcoTokioInsurance -> m ())
updateByPrimaryKey (Domain.Types.IffcoTokioInsurance.IffcoTokioInsurance {..}) = do {_now <- getCurrentTime;
                                                                                     updateWithKV [Se.Set Beam.certificateNumber certificateNumber,
                                                                                                   Se.Set Beam.declarationId declarationId,
                                                                                                   Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
                                                                                                   Se.Set Beam.iffcoStatus iffcoStatus,
                                                                                                   Se.Set Beam.insuranceStatus insuranceStatus,
                                                                                                   Se.Set Beam.invoiceRequestNumber invoiceRequestNumber,
                                                                                                   Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
                                                                                                   Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                                   Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



