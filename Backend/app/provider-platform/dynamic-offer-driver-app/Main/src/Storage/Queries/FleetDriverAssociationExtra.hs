{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FleetDriverAssociationExtra where

import qualified Database.Beam as B
import qualified Database.Beam.Query ()
import qualified Domain.Types.Common as DI
import Domain.Types.FleetDriverAssociation
import Domain.Types.Person
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Beam.FleetDriverAssociation as BeamFDVA
import Storage.Queries.OrphanInstances.FleetDriverAssociation ()

-- Extra code goes here --

upsert :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FleetDriverAssociation -> m ()
upsert a@FleetDriverAssociation {..} = do
  res <- findOneWithKV [Se.And [Se.Is BeamFDVA.driverId $ Se.Eq (a.driverId.getId), Se.Is BeamFDVA.fleetOwnerId $ Se.Eq a.fleetOwnerId]]
  if isJust res
    then
      updateOneWithKV
        [ Se.Set BeamFDVA.isActive isActive,
          Se.Set BeamFDVA.updatedAt updatedAt
        ]
        [Se.And [Se.Is BeamFDVA.driverId $ Se.Eq (a.driverId.getId), Se.Is BeamFDVA.fleetOwnerId $ Se.Eq a.fleetOwnerId]]
    else createWithKV a

findAllActiveDriverByFleetOwnerId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Int -> Int -> m [FleetDriverAssociation]
findAllActiveDriverByFleetOwnerId fleetOwnerId limit offset = do
  findAllWithOptionsKV
    [Se.And [Se.Is BeamFDVA.fleetOwnerId $ Se.Eq fleetOwnerId, Se.Is BeamFDVA.isActive $ Se.Eq True]]
    (Se.Desc BeamFDVA.updatedAt)
    (Just limit)
    (Just offset)

findAllDriverByFleetOwnerIdAndMbIsActive ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Text ->
  Maybe Bool ->
  Int ->
  Int ->
  m [FleetDriverAssociation]
findAllDriverByFleetOwnerIdAndMbIsActive fleetOwnerId mbIsActive limit offset = do
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is BeamFDVA.fleetOwnerId $ Se.Eq fleetOwnerId]
          <> [Se.Is BeamFDVA.isActive (Se.Eq isActive) | Just isActive <- [mbIsActive]]
    ]
    (Se.Desc BeamFDVA.updatedAt)
    (Just limit)
    (Just offset)

endFleetDriverAssociation :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Id Person -> m ()
endFleetDriverAssociation fleetOwnerId (Id driverId) = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set BeamFDVA.associatedTill $ Just now, Se.Set BeamFDVA.isActive False]
    [Se.And [Se.Is BeamFDVA.fleetOwnerId (Se.Eq fleetOwnerId), Se.Is BeamFDVA.associatedTill (Se.GreaterThan $ Just now), Se.Is BeamFDVA.driverId (Se.Eq driverId)]]

findAllDriversByFleetOwnerIdByMode :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DI.DriverMode -> Maybe Bool -> Integer -> Integer -> m [FleetDriverAssociation]
findAllDriversByFleetOwnerIdByMode fleetOwnerId mode mbIsActive limitVal offsetVal = do
  dbConf <- getMasterBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limitVal $
            B.offset_ offsetVal $
              B.orderBy_ (\(rc', _) -> B.desc_ rc'.createdAt) $
                B.filter_'
                  ( \(fleetDriverAssociation, driverInformation) ->
                      fleetDriverAssociation.fleetOwnerId B.==?. B.val_ fleetOwnerId
                        B.&&?. driverInformation.mode B.==?. B.val_ (Just mode)
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\isActive -> fleetDriverAssociation.isActive B.==?. B.val_ isActive) mbIsActive
                  )
                  do
                    fleetDriverAssociation <- B.all_ (BeamCommon.fleetDriverAssociation BeamCommon.atlasDB)
                    driverInformation <- B.join_ (BeamCommon.driverInformation BeamCommon.atlasDB) (\driverInfo -> BeamFDVA.driverId fleetDriverAssociation B.==. BeamDI.driverId driverInfo)
                    pure (fleetDriverAssociation, driverInformation)
  case res of
    Right res' -> do
      let fleetDriverList = fst <$> res'
      catMaybes <$> mapM fromTType' fleetDriverList
    Left _ -> pure []
