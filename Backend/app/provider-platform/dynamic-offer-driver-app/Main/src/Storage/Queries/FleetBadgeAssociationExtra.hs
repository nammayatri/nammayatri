{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetBadgeAssociationExtra where

import Control.Applicative (liftA2)
import Control.Lens ((^?), _head)
import qualified Data.Text as T
import qualified Database.Beam as B
import Domain.Types.FleetBadge
import Domain.Types.FleetBadgeAssociation
import Domain.Types.FleetBadgeType
import qualified Domain.Types.FleetBadgeType as DFBT
import Domain.Types.Person
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.FleetBadge as BeamFB
import qualified Storage.Beam.FleetBadgeAssociation as BeamFBA
import Storage.Queries.OrphanInstances.FleetBadge
import Storage.Queries.OrphanInstances.FleetBadgeAssociation

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => FleetBadgeAssociation -> m ()
create = createWithKV

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

findActiveFleetBadgeAssociationById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id FleetBadge -> FleetBadgeType -> m (Maybe FleetBadgeAssociation)
findActiveFleetBadgeAssociationById (Id fleetBadge) badgeType = do
  now <- getCurrentTime
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamFBA.badgeId $ Se.Eq fleetBadge,
          Se.Is BeamFBA.associatedTill (Se.GreaterThan $ Just now),
          Se.Is BeamFBA.isActive (Se.Eq True),
          Se.Is BeamFBA.badgeType $ Se.Eq badgeType
        ]
    ]
    (Se.Desc BeamFBA.associatedTill)
    (Just 1)
    Nothing
    <&> (^? _head)

findAllActiveFleetBadgeAssociation :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Int -> Maybe Int -> Maybe Text -> Maybe DFBT.FleetBadgeType -> m [(FleetBadgeAssociation, FleetBadge)]
findAllActiveFleetBadgeAssociation Nothing Nothing mbSearchString mbBadgeType = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.filter_'
          ( \(fba, badge) ->
              B.sqlBool_ (fba.associatedTill B.>=. B.val_ (Just now))
                B.&&?. (fba.isActive B.==?. B.val_ True)
                B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\badgeType -> badge.badgeType B.==?. B.val_ badgeType) mbBadgeType
                B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (B.upper_ badge.badgeName `B.like_` B.val_ ("%" <> T.toUpper searchStr <> "%"))) mbSearchString
          )
          $ do
            fleetBadgeAssociation <- B.all_ (BeamCommon.fleetBadgeAssociation BeamCommon.atlasDB)
            badge <- B.join_ (BeamCommon.fleetBadge BeamCommon.atlasDB) (\badge -> BeamFB.id badge B.==. BeamFBA.badgeId fleetBadgeAssociation)
            pure (fleetBadgeAssociation, badge)
  case res of
    Right res' -> catMaybes <$> mapM (\(fba, badge) -> liftA2 (,) <$> fromTType' fba <*> fromTType' badge) res'
    Left _ -> pure []
findAllActiveFleetBadgeAssociation mbLimit mbOffset mbSearchString mbBadgeType = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ (fromIntegral limit) $
          B.offset_ (fromIntegral offset) $
            B.filter_'
              ( \(fba, badge) ->
                  B.sqlBool_ (fba.associatedTill B.>=. B.val_ (Just now))
                    B.&&?. (fba.isActive B.==?. B.val_ True)
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\badgeType -> badge.badgeType B.==?. B.val_ badgeType) mbBadgeType
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (B.upper_ badge.badgeName `B.like_` B.val_ ("%" <> T.toUpper searchStr <> "%"))) mbSearchString
              )
              $ do
                fleetBadgeAssociation <- B.all_ (BeamCommon.fleetBadgeAssociation BeamCommon.atlasDB)
                badge <- B.join_ (BeamCommon.fleetBadge BeamCommon.atlasDB) (\badge -> BeamFB.id badge B.==. BeamFBA.badgeId fleetBadgeAssociation)
                pure (fleetBadgeAssociation, badge)
  case res of
    Right res' -> catMaybes <$> mapM (\(fba, badge) -> liftA2 (,) <$> fromTType' fba <*> fromTType' badge) res'
    Left _ -> pure []

findAllInactiveFleetBadgeAssociation :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Int -> Maybe Int -> Maybe Text -> Maybe DFBT.FleetBadgeType -> m [(FleetBadgeAssociation, FleetBadge)]
findAllInactiveFleetBadgeAssociation mbLimit mbOffset mbSearchString mbBadgeType = do
  allActive <- findAllActiveFleetBadgeAssociation Nothing Nothing mbSearchString mbBadgeType
  let activeIds = map (getId . badgeId . fst) allActive
  dbConf <- getReplicaBeamConfig
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ (fromIntegral limit) $
          B.offset_ (fromIntegral offset) $
            B.filter_'
              ( \(fba, badge) ->
                  B.sqlBool_ (B.not_ (fba.badgeId `B.in_` (B.val_ <$> activeIds)))
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\badgeType -> badge.badgeType B.==?. B.val_ badgeType) mbBadgeType
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (B.upper_ badge.badgeName `B.like_` B.val_ ("%" <> T.toUpper searchStr <> "%"))) mbSearchString
              )
              $ do
                fleetBadgeAssociation <- B.all_ (BeamCommon.fleetBadgeAssociation BeamCommon.atlasDB)
                badge <- B.join_ (BeamCommon.fleetBadge BeamCommon.atlasDB) (\badge -> BeamFB.id badge B.==. BeamFBA.badgeId fleetBadgeAssociation)
                pure (fleetBadgeAssociation, badge)
  case res of
    Right res' -> catMaybes <$> mapM (\(fba, badge) -> liftA2 (,) <$> fromTType' fba <*> fromTType' badge) res'
    Left _ -> pure []

createBadgeAssociationIfNotExists :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FleetBadgeAssociation -> m ()
createBadgeAssociationIfNotExists badgeAssociation = do
  existingBadgeAssociation <- findActiveFleetBadgeAssociationById badgeAssociation.badgeId badgeAssociation.badgeType
  case existingBadgeAssociation of
    Just _ -> pure ()
    Nothing -> do
      create badgeAssociation
      pure ()
