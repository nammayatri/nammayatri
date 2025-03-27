{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetBadgeAssociationExtra where

import Domain.Types.FleetBadge
import Domain.Types.FleetBadgeAssociation
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetBadgeAssociation as BeamFBA
import Storage.Queries.OrphanInstances.FleetBadgeAssociation

findActiveFleetBadgeByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m (Maybe FleetBadgeAssociation)
findActiveFleetBadgeByDriverId (Id driverId) = do
  now <- getCurrentTime
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamFBA.driverId $ Se.Eq driverId,
          Se.Is BeamFBA.isActive $ Se.Eq True,
          Se.Is BeamFBA.associatedTill (Se.GreaterThan $ Just now)
        ]
    ]
    (Se.Desc BeamFBA.associatedTill)
    (Just 1)
    Nothing
    <&> listToMaybe

endAssociationForDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m ()
endAssociationForDriver (Id driverId) = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set BeamFBA.associatedTill $ Just now, Se.Set BeamFBA.isActive False]
    [ Se.And
        [ Se.Is BeamFBA.driverId (Se.Eq driverId),
          Se.Is BeamFBA.associatedTill (Se.GreaterThan $ Just now),
          Se.Is BeamFBA.isActive (Se.Eq True)
        ]
    ]

findActiveFleetBadgeAssociationById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id FleetBadge -> m (Maybe FleetBadgeAssociation)
findActiveFleetBadgeAssociationById (Id fleetBadge) = do
  now <- getCurrentTime
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamFBA.badgeId $ Se.Eq fleetBadge,
          Se.Is BeamFBA.associatedTill (Se.GreaterThan $ Just now),
          Se.Is BeamFBA.isActive (Se.Eq True)
        ]
    ]
    (Se.Desc BeamFBA.associatedTill)
    (Just 1)
    Nothing
    <&> listToMaybe
