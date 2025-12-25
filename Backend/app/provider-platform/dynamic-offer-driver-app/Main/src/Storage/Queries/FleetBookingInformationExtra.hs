{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetBookingInformationExtra where

import Data.Text (toLower)
import qualified Database.Beam as B
import qualified Domain.Types.FleetBookingInformation
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.FleetBookingInformation as Beam
import Storage.Queries.OrphanInstances.FleetBookingInformation

-- Extra code goes here --
findAllByFleetOwnerIdsAndFilters :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Text] -> Maybe Kernel.Prelude.UTCTime -> Maybe Kernel.Prelude.UTCTime -> Maybe Int -> Maybe Int -> Bool -> Maybe Text -> Maybe Text -> m [Domain.Types.FleetBookingInformation.FleetBookingInformation]
findAllByFleetOwnerIdsAndFilters fleetOwnersIds from' to' limit offset searchByFleetOwnerId ticketPlaceId mbStatus =
  findAllWithOptionsKV
    [ Se.And $
        (if searchByFleetOwnerId && mbStatus /= Just "NEW" then [Se.Is Beam.fleetOwnerId $ Se.In $ map pure fleetOwnersIds] else [])
          <> (if isJust ticketPlaceId then [Se.Is Beam.ticketPlaceId $ Se.Eq ticketPlaceId] else [])
          <> (if isJust mbStatus then [Se.Is Beam.status $ Se.Eq mbStatus] else [])
          <> ( case (from', to') of
                 (Just from, Just to) ->
                   [ Se.Is Beam.createdAt $ Se.GreaterThanOrEq from,
                     Se.Is Beam.createdAt $ Se.LessThan to
                   ]
                 (_, _) -> []
             )
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findFleetBookingInformationByFleetOwnerIdsAndFilters :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Text] -> Maybe UTCTime -> Maybe UTCTime -> Maybe Int -> Maybe Int -> Bool -> Maybe Text -> Maybe Text -> Maybe Text -> m [Domain.Types.FleetBookingInformation.FleetBookingInformation]
findFleetBookingInformationByFleetOwnerIdsAndFilters fleetOwnerIds from' to' limit offset searchByFleetOwnerId ticketPlaceId vehicleNo status = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral $ fromMaybe 1000 limit) $
            B.offset_ (fromIntegral $ fromMaybe 0 offset) $
              B.orderBy_ (\fb' -> B.desc_ fb'.createdAt) $
                B.filter_'
                  ( \fb ->
                      (if searchByFleetOwnerId then (B.sqlBool_ $ fb.fleetOwnerId `B.in_` ((B.val_ . Just) <$> fleetOwnerIds)) else B.sqlBool_ $ B.val_ True)
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\s -> fb.status B.==?. B.val_ (Just s)) status
                        B.&&?. (maybe (B.sqlBool_ $ B.val_ True) (\tPlaceId -> fb.ticketPlaceId B.==?. B.val_ (Just tPlaceId)) ticketPlaceId)
                        B.&&?. ( case (from', to') of
                                   (Just from, Just to) ->
                                     B.sqlBool_ (fb.createdAt B.>=. B.val_ from) B.&&?. B.sqlBool_ (fb.createdAt B.<=. B.val_ to)
                                   (_, _) -> B.sqlBool_ $ B.val_ True
                               )
                        B.&&?. (maybe (B.sqlBool_ $ B.val_ True) (\vNo -> B.sqlBool_ $ B.like_ (B.lower_ (B.coalesce_ [fb.vehicleNo] (B.val_ ""))) (B.val_ ("%" <> toLower vNo <> "%"))) vehicleNo)
                  )
                  $ B.all_ (BeamCommon.fleetBookingInformation BeamCommon.atlasDB)
  case res of
    Right res' -> do
      catMaybes <$> mapM fromTType' res'
    Left _ -> pure []
