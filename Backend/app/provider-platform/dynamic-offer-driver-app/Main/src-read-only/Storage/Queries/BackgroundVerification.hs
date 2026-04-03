{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.BackgroundVerification (module Storage.Queries.BackgroundVerification, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.BackgroundVerificationExtra as ReExport
import qualified Domain.Types.BackgroundVerification
import qualified Storage.Beam.BackgroundVerification as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Kernel.Types.Documents
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BackgroundVerification.BackgroundVerification -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BackgroundVerification.BackgroundVerification] -> m ())
createMany = traverse_ create
findByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.BackgroundVerification.BackgroundVerification))
findByDriverId driverId = do findOneWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
updateInvitationStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Documents.VerificationStatus -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateInvitationStatus invitationStatus driverId = do {_now <- getCurrentTime;
                                                       updateOneWithKV [Se.Set Beam.invitationStatus invitationStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]}
updateReportId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateReportId reportId driverId = do {_now <- getCurrentTime;
                                       updateOneWithKV [Se.Set Beam.reportId reportId, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]}
updateReportStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Documents.VerificationStatus -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateReportStatus reportStatus driverId = do {_now <- getCurrentTime;
                                               updateOneWithKV [Se.Set Beam.reportStatus reportStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]}
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.BackgroundVerification.BackgroundVerification))
findByPrimaryKey driverId = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BackgroundVerification.BackgroundVerification -> m ())
updateByPrimaryKey (Domain.Types.BackgroundVerification.BackgroundVerification {..}) = do {_now <- getCurrentTime;
                                                                                           updateWithKV [Se.Set Beam.candidateId candidateId,
                                                                                                         Se.Set Beam.expiresAt expiresAt,
                                                                                                         Se.Set Beam.invitationId invitationId,
                                                                                                         Se.Set Beam.invitationStatus invitationStatus,
                                                                                                         Se.Set Beam.invitationUrl (Kernel.Prelude.showBaseUrl invitationUrl),
                                                                                                         Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
                                                                                                         Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                                         Se.Set Beam.reportId reportId,
                                                                                                         Se.Set Beam.reportStatus reportStatus,
                                                                                                         Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]}



