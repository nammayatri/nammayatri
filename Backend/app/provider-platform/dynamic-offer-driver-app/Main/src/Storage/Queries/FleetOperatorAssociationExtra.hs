module Storage.Queries.FleetOperatorAssociationExtra where

import Domain.Types.FleetOperatorAssociation
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetOperatorAssociation as Beam
import qualified Storage.Beam.FleetOperatorAssociation as BeamFOA
import Storage.Queries.OrphanInstances.FleetOperatorAssociation ()

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

findAllByFleetOwnerId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  Bool ->
  m [FleetOperatorAssociation]
findAllByFleetOwnerId fleetOwnerId isActive = do
  now <- getCurrentTime
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamFOA.fleetOwnerId $ Se.Eq fleetOwnerId.getId,
          Se.Is BeamFOA.isActive $ Se.Eq isActive,
          Se.Is BeamFOA.associatedTill (Se.GreaterThan $ Just now)
        ]
    ]
    (Se.Desc BeamFOA.createdAt)
    Nothing
    Nothing

-- including inactive
findAllByFleetIdAndOperatorId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  Id DP.Person ->
  m [FleetOperatorAssociation]
findAllByFleetIdAndOperatorId fleetOwnerId operatorId = do
  now <- getCurrentTime
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamFOA.fleetOwnerId $ Se.Eq fleetOwnerId.getId,
          Se.Is BeamFOA.operatorId $ Se.Eq operatorId.getId,
          Se.Is BeamFOA.associatedTill (Se.GreaterThan $ Just now)
        ]
    ]
    (Se.Desc BeamFOA.createdAt)
    Nothing
    Nothing

endFleetOperatorAssociation :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Id DP.Person -> m ()
endFleetOperatorAssociation fleetOwnerId operatorId = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set BeamFOA.associatedTill $ Just now, Se.Set BeamFOA.isActive False]
    [ Se.And
        [ Se.Is BeamFOA.operatorId (Se.Eq operatorId.getId),
          Se.Is BeamFOA.associatedTill (Se.GreaterThan $ Just now),
          Se.Is BeamFOA.fleetOwnerId (Se.Eq fleetOwnerId.getId)
        ]
    ]

findByFleetOwnerIdAndOperatorId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  Id DP.Person ->
  Bool ->
  m (Maybe FleetOperatorAssociation)
findByFleetOwnerIdAndOperatorId fleetOwnerId operatorId isActive = do
  now <- getCurrentTime
  listToMaybe
    <$> findAllWithOptionsKV
      [ Se.And
          [ Se.Is BeamFOA.fleetOwnerId $ Se.Eq fleetOwnerId.getId,
            Se.Is BeamFOA.operatorId $ Se.Eq operatorId.getId,
            Se.Is BeamFOA.isActive $ Se.Eq isActive,
            Se.Is BeamFOA.associatedTill (Se.GreaterThan $ Just now)
          ]
      ]
      (Se.Desc BeamFOA.createdAt)
      (Just 1)
      Nothing
