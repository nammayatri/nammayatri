module Storage.Queries.FleetOperatorAssociationExtra where

import Domain.Types.FleetOperatorAssociation (FleetOperatorAssociation)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetOperatorAssociation as Beam
import Storage.Queries.FleetOperatorAssociation ()

findAllActiveByOperatorIdWithLimitOffset ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Maybe Int ->
  Maybe Int ->
  m [FleetOperatorAssociation]
findAllActiveByOperatorIdWithLimitOffset operatorId mbLimit mbOffset =
  findAllWithOptionsKV
    [Se.Is Beam.operatorId $ Se.Eq operatorId, Se.Is Beam.isActive $ Se.Eq True]
    (Se.Asc Beam.associatedOn)
    (Just . min 10 . fromMaybe 5 $ mbLimit)
    (Just $ fromMaybe 0 mbOffset)
