{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.StationExtra where

import Data.Either
import qualified Database.Beam as B
import Domain.Types.MerchantOperatingCity
import Domain.Types.Station
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Station as BeamS
import Storage.Queries.OrphanInstances.Station

findAllMatchingStations :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Text -> Integer -> Integer -> Id MerchantOperatingCity -> m [Station]
findAllMatchingStations mbSearchStr limitVal offsetVal (Id merchantOperatingCityId') = do
  dbConf <- getMasterBeamConfig
  stations <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limitVal $
            B.offset_ offsetVal $
              B.filter_'
                ( \BeamS.StationT {..} ->
                    merchantOperatingCityId B.==?. B.val_ merchantOperatingCityId'
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (name `B.like_` B.val_ ("%" <> searchStr <> "%"))) mbSearchStr
                )
                $ B.all_ (BeamCommon.station BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] stations)
