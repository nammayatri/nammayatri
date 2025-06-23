{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetBadgeExtra where

import Data.Either
import qualified Data.Text as T
import qualified Database.Beam as B
import Domain.Types.FleetBadge
import Domain.Types.FleetBadgeType
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.FleetBadge as BeamFB
import Storage.Queries.OrphanInstances.FleetBadge

findAllMatchingBadges :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Text -> Maybe Integer -> Maybe Integer -> Id MerchantOperatingCity -> Text -> Maybe FleetBadgeType -> m [FleetBadge]
findAllMatchingBadges mbSearchStr (Just limitVal) (Just offsetVal) (Id merchantOperatingCityId') fleetOwnerId' mbBadgeType = do
  dbConf <- getReplicaBeamConfig
  fleetBadges <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limitVal $
            B.offset_ offsetVal $
              B.filter_'
                ( \BeamFB.FleetBadgeT {..} ->
                    merchantOperatingCityId B.==?. B.val_ merchantOperatingCityId'
                      B.&&?. fleetOwnerId B.==?. B.val_ fleetOwnerId'
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\badgeType' -> badgeType B.==?. B.val_ badgeType') mbBadgeType
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (B.upper_ badgeName `B.like_` B.val_ ("%" <> T.toUpper searchStr <> "%"))) mbSearchStr
                )
                $ B.all_ (BeamCommon.fleetBadge BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] fleetBadges)
findAllMatchingBadges mbSearchStr _ _ (Id merchantOperatingCityId') fleetOwnerId' mbBadgeType = do
  dbConf <- getReplicaBeamConfig
  fleetBadges <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.filter_'
            ( \BeamFB.FleetBadgeT {..} ->
                merchantOperatingCityId B.==?. B.val_ merchantOperatingCityId'
                  B.&&?. fleetOwnerId B.==?. B.val_ fleetOwnerId'
                  B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\badgeType' -> badgeType B.==?. B.val_ badgeType') mbBadgeType
                  B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (B.upper_ badgeName `B.like_` B.val_ ("%" <> T.toUpper searchStr <> "%"))) mbSearchStr
            )
            $ B.all_ (BeamCommon.fleetBadge BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] fleetBadges)

findAllMatchingBadgesMultiFleetOwner :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Text -> Maybe Integer -> Maybe Integer -> Id MerchantOperatingCity -> [Text] -> Maybe FleetBadgeType -> m [FleetBadge]
findAllMatchingBadgesMultiFleetOwner mbSearchStr (Just limitVal) (Just offsetVal) (Id merchantOperatingCityId') fleetOwnerIds' mbBadgeType = do
  dbConf <- getReplicaBeamConfig
  fleetBadges <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limitVal $
            B.offset_ offsetVal $
              B.filter_'
                ( \BeamFB.FleetBadgeT {..} ->
                    merchantOperatingCityId B.==?. B.val_ merchantOperatingCityId'
                      B.&&?. (B.sqlBool_ $ fleetOwnerId `B.in_` (map B.val_ fleetOwnerIds'))
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\badgeType' -> badgeType B.==?. B.val_ badgeType') mbBadgeType
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (B.upper_ badgeName `B.like_` B.val_ ("%" <> T.toUpper searchStr <> "%"))) mbSearchStr
                )
                $ B.all_ (BeamCommon.fleetBadge BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] fleetBadges)
findAllMatchingBadgesMultiFleetOwner mbSearchStr _ _ (Id merchantOperatingCityId') fleetOwnerIds' mbBadgeType = do
  dbConf <- getReplicaBeamConfig
  fleetBadges <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.filter_'
            ( \BeamFB.FleetBadgeT {..} ->
                merchantOperatingCityId B.==?. B.val_ merchantOperatingCityId'
                  B.&&?. (B.sqlBool_ $ fleetOwnerId `B.in_` (map B.val_ fleetOwnerIds'))
                  B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\badgeType' -> badgeType B.==?. B.val_ badgeType') mbBadgeType
                  B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (B.upper_ badgeName `B.like_` B.val_ ("%" <> T.toUpper searchStr <> "%"))) mbSearchStr
            )
            $ B.all_ (BeamCommon.fleetBadge BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] fleetBadges)
