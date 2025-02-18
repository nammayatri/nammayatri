{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.RouteExtra where

import BecknV2.FRFS.Enums
import Data.Either
import qualified Data.Text as T
import qualified Database.Beam as B
import Domain.Types.MerchantOperatingCity
import Domain.Types.Route
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Route as BeamR
import Storage.Queries.OrphanInstances.Route ()

findAllMatchingRoutes :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Text -> Maybe Integer -> Maybe Integer -> Id MerchantOperatingCity -> VehicleCategory -> m [Route]
findAllMatchingRoutes mbSearchStr (Just limitVal) (Just offsetVal) (Id merchantOperatingCityId') vehicle = do
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
                      B.&&?. ( maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (B.upper_ code `B.like_` B.val_ ("%" <> T.toUpper searchStr <> "%"))) mbSearchStr
                                 B.||?. maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (B.upper_ shortName `B.like_` B.val_ ("%" <> T.toUpper searchStr <> "%"))) mbSearchStr
                                 B.||?. maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (B.upper_ longName `B.like_` B.val_ ("%" <> T.toUpper searchStr <> "%"))) mbSearchStr
                             )
                )
                $ B.all_ (BeamCommon.route BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] routes)
findAllMatchingRoutes mbSearchStr _ _ (Id merchantOperatingCityId') vehicle = do
  dbConf <- getReplicaBeamConfig
  routes <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.filter_'
            ( \BeamR.RouteT {..} ->
                merchantOperatingCityId B.==?. B.val_ merchantOperatingCityId'
                  B.&&?. vehicleType B.==?. B.val_ vehicle
                  B.&&?. ( maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (B.upper_ code `B.like_` B.val_ ("%" <> T.toUpper searchStr <> "%"))) mbSearchStr
                             B.||?. maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (B.upper_ shortName `B.like_` B.val_ ("%" <> T.toUpper searchStr <> "%"))) mbSearchStr
                             B.||?. maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (B.upper_ longName `B.like_` B.val_ ("%" <> T.toUpper searchStr <> "%"))) mbSearchStr
                         )
            )
            $ B.all_ (BeamCommon.route BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] routes)
