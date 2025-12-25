{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RouteExtra where

import Data.Either
import qualified Data.Text as T
import qualified Database.Beam as B
import Domain.Types.MerchantOperatingCity
import Domain.Types.Route
import Domain.Types.VehicleCategory
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Route as BeamR
import Storage.Queries.OrphanInstances.Route ()

-- Extra code goes here --
findAllMatchingRoutes :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Text -> Maybe Integer -> Maybe Integer -> Id MerchantOperatingCity -> VehicleCategory -> [Text] -> m [Route]
findAllMatchingRoutes mbSearchStr (Just limitVal) (Just offsetVal) (Id merchantOperatingCityId') vehicle routeCodes = do
  dbConf <- getReplicaBeamConfig
  routes <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limitVal $
            B.offset_ offsetVal $
              B.filter_'
                ( \BeamR.RouteT {..} ->
                    merchantOperatingCityId B.==?. B.val_ merchantOperatingCityId'
                      B.&&?. vehicleType B.==?. B.val_ vehicle
                      B.&&?. B.sqlBool_ (code `B.in_` (B.val_ <$> routeCodes))
                      B.&&?. ( maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (B.upper_ code `B.like_` B.val_ ("%" <> T.toUpper searchStr <> "%"))) mbSearchStr
                                 B.||?. maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (B.upper_ shortName `B.like_` B.val_ ("%" <> T.toUpper searchStr <> "%"))) mbSearchStr
                                 B.||?. maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (B.upper_ longName `B.like_` B.val_ ("%" <> T.toUpper searchStr <> "%"))) mbSearchStr
                             )
                )
                $ B.all_ (BeamCommon.route BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] routes)
findAllMatchingRoutes mbSearchStr _ _ (Id merchantOperatingCityId') vehicle routeCodes = do
  dbConf <- getReplicaBeamConfig
  routes <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.filter_'
            ( \BeamR.RouteT {..} ->
                merchantOperatingCityId B.==?. B.val_ merchantOperatingCityId'
                  B.&&?. vehicleType B.==?. B.val_ vehicle
                  B.&&?. B.sqlBool_ (code `B.in_` (B.val_ <$> routeCodes))
                  B.&&?. ( maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (B.upper_ code `B.like_` B.val_ ("%" <> T.toUpper searchStr <> "%"))) mbSearchStr
                             B.||?. maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (B.upper_ shortName `B.like_` B.val_ ("%" <> T.toUpper searchStr <> "%"))) mbSearchStr
                             B.||?. maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (B.upper_ longName `B.like_` B.val_ ("%" <> T.toUpper searchStr <> "%"))) mbSearchStr
                         )
            )
            $ B.all_ (BeamCommon.route BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] routes)
