module Storage.Queries.IffcoTokioInsuranceExtra where

import qualified Domain.Types.IffcoTokioInsurance
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.IffcoTokioInsurance as Beam
import Storage.Queries.OrphanInstances.IffcoTokioInsurance ()

-- Extra code goes here --

-- | Find the most recent insurance entry for a driver, sorted newest first
findLatestByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id Domain.Types.Person.Person ->
  m (Maybe Domain.Types.IffcoTokioInsurance.IffcoTokioInsurance)
findLatestByDriverId (Id driverId) = do
  results <- findAllWithOptionsKV [Se.Is Beam.driverId $ Se.Eq driverId] (Se.Desc Beam.createdAt) (Just 1) Nothing
  pure $ listToMaybe results

-- | Update an entry by invoiceRequestNumber with async response values
updateByInvoiceRequestNumber ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Domain.Types.IffcoTokioInsurance.IffcoTokioInsuranceStatus ->
  m ()
updateByInvoiceRequestNumber invoiceReqNum certNum declId iffcoSt insStatus = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.certificateNumber certNum,
      Se.Set Beam.declarationId declId,
      Se.Set Beam.iffcoStatus iffcoSt,
      Se.Set Beam.insuranceStatus insStatus,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.invoiceRequestNumber $ Se.Eq invoiceReqNum]
