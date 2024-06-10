{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MessageReport (module Storage.Queries.MessageReport, module ReExport) where

import qualified Domain.Types.Message
import qualified Domain.Types.MessageReport
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MessageReport as Beam
import Storage.Queries.MessageReportExtra as ReExport
import Storage.Queries.Transformers.MessageReport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MessageReport.MessageReport -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MessageReport.MessageReport] -> m ())
createMany = traverse_ create

deleteByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Driver -> m ())
deleteByPersonId driverId = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Message.Message -> m (Maybe Domain.Types.MessageReport.MessageReport))
findById messageId = do findOneWithKV [Se.Is Beam.messageId $ Se.Eq (Kernel.Types.Id.getId messageId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Driver -> Kernel.Types.Id.Id Domain.Types.Message.Message -> m (Maybe Domain.Types.MessageReport.MessageReport))
findByPrimaryKey driverId messageId = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.messageId $ Se.Eq (Kernel.Types.Id.getId messageId)]]
