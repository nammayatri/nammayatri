{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Issue (module Storage.Queries.Issue, module ReExport) where

import qualified Domain.Types.Issue
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Issue as Beam
import Storage.Queries.IssueExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.Issue.Issue -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.Issue.Issue] -> m ())
createMany = traverse_ create

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Issue.Issue -> m (Maybe Domain.Types.Issue.Issue))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.Issue.Issue -> m ())
updateByPrimaryKey (Domain.Types.Issue.Issue {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bookingId (Kernel.Types.Id.getId <$> bookingId),
      Se.Set Beam.contactEmail contactEmail,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.customerId (Kernel.Types.Id.getId customerId),
      Se.Set Beam.description description,
      Se.Set Beam.nightSafety nightSafety,
      Se.Set Beam.reason reason,
      Se.Set Beam.status status,
      Se.Set Beam.ticketId ticketId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
