{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetMemberAssociationExtra where

import qualified Domain.Types.FleetMemberAssociation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetMemberAssociation as Beam
import Storage.Queries.OrphanInstances.FleetMemberAssociation

-- Extra code goes here --

findOneByFleetOwnerId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Bool -> m (Maybe Domain.Types.FleetMemberAssociation.FleetMemberAssociation))
findOneByFleetOwnerId fleetOwnerId isFleetOwner = do findAllWithKV [Se.And [Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId, Se.Is Beam.isFleetOwner $ Se.Eq isFleetOwner]] <&> listToMaybe

-- | Every member row of the given fleets, read straight from the database rather than the key-value
-- store, so a backfill sees all rows and not only the ones recently cached.
findAllByFleetOwnerIdsDb :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Kernel.Prelude.Text] -> m [Domain.Types.FleetMemberAssociation.FleetMemberAssociation]
findAllByFleetOwnerIdsDb fleetOwnerIds = findAllWithDb [Se.Is Beam.fleetOwnerId $ Se.In fleetOwnerIds]
