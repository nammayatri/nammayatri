{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetBookingAssignmentsExtra where

import qualified Domain.Types.FleetBookingAssignments as DBFBA
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetBookingAssignments as Beam
import Storage.Queries.OrphanInstances.FleetBookingAssignments

-- Extra code goes here --

findAllByFleetOwnerIdsAndFilters ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [Text] -> -- fleetOwnerIds
  Maybe Text -> -- mainAssignmentId
  Maybe UTCTime -> -- from'
  Maybe UTCTime -> -- to'
  Maybe Int -> -- limit
  Maybe Int -> -- offset
  Maybe Text -> -- vehicleNos
  m [DBFBA.FleetBookingAssignments]
findAllByFleetOwnerIdsAndFilters fleetOwnerIds mainAssignmentId' from' to' mLimit mOffset mVehicleNo =
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is Beam.fleetOwnerId $ Se.In fleetOwnerIds]
          <> ( case mainAssignmentId' of
                 Just mainAssignmentId -> [Se.Is Beam.mainAssignmentId $ Se.Eq mainAssignmentId]
                 Nothing -> []
             )
          <> ( case (from', to') of
                 (Just from, Just to) ->
                   [ Se.Is Beam.createdAt $ Se.GreaterThanOrEq from,
                     Se.Is Beam.createdAt $ Se.LessThanOrEq to
                   ]
                 (Just from, Nothing) ->
                   [ Se.Is Beam.createdAt $ Se.GreaterThanOrEq from
                   ]
                 (Nothing, Just to) ->
                   [ Se.Is Beam.createdAt $ Se.LessThanOrEq to
                   ]
                 (Nothing, Nothing) -> []
             )
          <> ( case mVehicleNo of
                 Just vehicleNo -> [Se.Is Beam.vehicleNo $ Se.Eq vehicleNo]
                 Nothing -> []
             )
    ]
    (Se.Desc Beam.createdAt)
    mLimit
    mOffset
