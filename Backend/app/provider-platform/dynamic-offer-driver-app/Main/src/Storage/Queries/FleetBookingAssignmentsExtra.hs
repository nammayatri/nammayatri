{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetBookingAssignmentsExtra where

import Data.Text (toLower)
import qualified Database.Beam as B
import qualified Domain.Types.FleetBookingAssignments as DBFBA
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.FleetBookingAssignments as Beam
import Storage.Queries.OrphanInstances.FleetBookingAssignments

-- Extra code goes here --

-- Get all assignments for the given vehicle numbers
findAllByVehicleNos ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [Text] -> -- vehicleNos
  m [DBFBA.FleetBookingAssignments]
findAllByVehicleNos vehicleNos = do
  findAllWithKV [Se.Is Beam.vehicleNo $ Se.In vehicleNos]

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

findFleetBookingAssignmentsByFleetOwnerIdsAndFilters :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Text] -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Int -> Maybe Int -> Maybe Text -> m [DBFBA.FleetBookingAssignments]
findFleetBookingAssignmentsByFleetOwnerIdsAndFilters fleetOwnerIds mainAssignmentId' from' to' limit offset vehicleNo = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral $ fromMaybe 1000 limit) $
            B.offset_ (fromIntegral $ fromMaybe 0 offset) $
              B.orderBy_ (\fba' -> B.desc_ fba'.createdAt) $
                B.filter_'
                  ( \fba ->
                      (B.sqlBool_ $ fba.fleetOwnerId `B.in_` ((B.val_) <$> fleetOwnerIds))
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\mainId -> fba.mainAssignmentId B.==?. B.val_ (mainId)) mainAssignmentId'
                        B.&&?. ( case (from', to') of
                                   (Just from, Just to) ->
                                     B.sqlBool_ (fba.createdAt B.>=. B.val_ from) B.&&?. B.sqlBool_ (fba.createdAt B.<=. B.val_ to)
                                   (Just from, Nothing) ->
                                     B.sqlBool_ (fba.createdAt B.>=. B.val_ from)
                                   (Nothing, Just to) ->
                                     B.sqlBool_ (fba.createdAt B.<=. B.val_ to)
                                   (Nothing, Nothing) -> B.sqlBool_ $ B.val_ True
                               )
                        B.&&?. (maybe (B.sqlBool_ $ B.val_ True) (\vNo -> B.sqlBool_ (B.lower_ fba.vehicleNo `B.like_` (B.val_ ("%" <> toLower vNo <> "%")))) vehicleNo)
                  )
                  $ B.all_ (BeamCommon.fleetBookingAssignments BeamCommon.atlasDB)
  case res of
    Right res' -> do
      catMaybes <$> mapM fromTType' res'
    Left _ -> pure []
