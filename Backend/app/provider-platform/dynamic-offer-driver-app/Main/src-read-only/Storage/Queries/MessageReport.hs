{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.MessageReport (module Storage.Queries.MessageReport, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.MessageReportExtra as ReExport
import Storage.Queries.Transformers.MessageReport
import qualified Domain.Types.MessageReport
import qualified Storage.Beam.MessageReport as Beam
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Domain.Types.Message
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MessageReport.MessageReport -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MessageReport.MessageReport] -> m ())
createMany = traverse_ create
deleteByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Driver -> m ())
deleteByPersonId driverId = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Message.Message -> m (Maybe Domain.Types.MessageReport.MessageReport))
findById messageId = do findOneWithKV [Se.Is Beam.messageId $ Se.Eq (Kernel.Types.Id.getId messageId)]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Kernel.Types.Id.Id Domain.Types.Person.Driver -> Kernel.Types.Id.Id Domain.Types.Message.Message -> m (Maybe Domain.Types.MessageReport.MessageReport))
findByPrimaryKey driverId messageId = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.messageId $ Se.Eq (Kernel.Types.Id.getId messageId)]]



