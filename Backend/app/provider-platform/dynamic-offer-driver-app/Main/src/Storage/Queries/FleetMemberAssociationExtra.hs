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
import qualified Storage.Beam.Person as BeamP
import Storage.Queries.OrphanInstances.FleetMemberAssociation

-- Extra code goes here --

findOneByFleetOwnerId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Bool -> m (Maybe Domain.Types.FleetMemberAssociation.FleetMemberAssociation))
findOneByFleetOwnerId fleetOwnerId isFleetOwner = do findAllWithKV [Se.And [Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId, Se.Is Beam.isFleetOwner $ Se.Eq isFleetOwner]] <&> listToMaybe

findAllActiveByFleetOwnerIdWithLimitOffsetSearch ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) =>
  Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  m [Domain.Types.FleetMemberAssociation.FleetMemberAssociation]
findAllActiveByFleetOwnerIdWithLimitOffsetSearch fleetOwnerId mbLimit mbOffset mbFrom mbTo = do
  now <- getCurrentTime
  let filters =
        catMaybes
          [ Just $ Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId,
            Just $ Se.Is Beam.enabled $ Se.Eq True,
            Just $ Se.Is Beam.associatedTill (Se.GreaterThan $ Just now),
            (\from -> Se.Is Beam.createdAt $ Se.GreaterThanOrEq from) <$> mbFrom,
            (\to -> Se.Is Beam.createdAt $ Se.LessThanOrEq to) <$> mbTo
          ]

      limit = min 10 $ fromMaybe 5 mbLimit
      offset = fromMaybe 0 mbOffset

  findAllWithOptionsDb
    [Se.And filters]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)
