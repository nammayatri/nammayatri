{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RouteExtra where

import Data.Either
import qualified Database.Beam as B
import Domain.Types.MerchantOperatingCity
import Domain.Types.Route
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Route as BeamR
import Storage.Queries.OrphanInstances.Route

findAllMatchingRoutes :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Text -> Integer -> Integer -> Id MerchantOperatingCity -> m [Route]
findAllMatchingRoutes mbSearchStr limitVal offsetVal (Id merchantOperatingCityId') = do
  dbConf <- getMasterBeamConfig
  routes <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limitVal $
            B.offset_ offsetVal $
              B.filter_'
                ( \BeamR.RouteT {..} ->
                    merchantOperatingCityId B.==?. B.val_ merchantOperatingCityId'
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (longName `B.like_` B.val_ ("%" <> searchStr <> "%"))) mbSearchStr
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (shortName `B.like_` B.val_ ("%" <> searchStr <> "%"))) mbSearchStr
                )
                $ B.all_ (BeamCommon.route BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] routes)
