{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.Issue (module Storage.Queries.Issue, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.IssueExtra as ReExport
import qualified Domain.Types.Issue
import qualified Storage.Beam.Issue as Beam
import qualified Kernel.Types.Id
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Issue.Issue -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Issue.Issue] -> m ())
createMany = traverse_ create
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Issue.Issue -> m (Maybe Domain.Types.Issue.Issue))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Issue.Issue -> m ())
updateByPrimaryKey (Domain.Types.Issue.Issue {..}) = do {_now <- getCurrentTime;
                                                         updateWithKV [Se.Set Beam.bookingId (Kernel.Types.Id.getId <$> bookingId),
                                                                       Se.Set Beam.contactEmail contactEmail,
                                                                       Se.Set Beam.customerId (Kernel.Types.Id.getId customerId),
                                                                       Se.Set Beam.description description,
                                                                       Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
                                                                       Se.Set Beam.nightSafety nightSafety,
                                                                       Se.Set Beam.reason reason,
                                                                       Se.Set Beam.status status,
                                                                       Se.Set Beam.ticketId ticketId,
                                                                       Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



