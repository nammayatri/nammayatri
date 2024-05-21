{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverGoHomeRequestExtra where

import Data.Time (UTCTime (..))
import Database.Beam as B
import qualified Database.Beam as B
import qualified Database.Beam.Postgres as B
import qualified Domain.Types.DriverGoHomeRequest as DDGR
import Domain.Types.Person
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Beam.Functions (findAllWithKV, findAllWithOptionsKV, findOneWithKV, getMasterBeamConfig, updateOneWithKV)
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id as Id
import Kernel.Utils.Common
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import Storage.Beam.DriverGoHomeRequest as BeamDHR
import Storage.Queries.OrphanInstances.DriverGoHomeRequest

-- Extra code goes here --

todaySuccessCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> m Int
todaySuccessCount driverId = do
  now <- getCurrentTime
  length <$> findAllWithKV [Se.Is BeamDHR.driverId $ Se.Eq $ getId driverId, Se.Is BeamDHR.status $ Se.Eq DDGR.SUCCESS, Se.Is BeamDHR.createdAt $ Se.GreaterThanOrEq now {utctDayTime = 0}, Se.Is BeamDHR.createdAt $ Se.LessThanOrEq now {utctDayTime = 86400}]

findActive :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Log m) => Id Driver -> m (Maybe DDGR.DriverGoHomeRequest)
findActive (Id.Id driverId) = findAllWithOptionsKV [Se.And [Se.Is BeamDHR.driverId $ Se.Eq driverId, Se.Is BeamDHR.status $ Se.Eq DDGR.ACTIVE]] (Se.Desc BeamDHR.createdAt) (Just 1) Nothing <&> listToMaybe
