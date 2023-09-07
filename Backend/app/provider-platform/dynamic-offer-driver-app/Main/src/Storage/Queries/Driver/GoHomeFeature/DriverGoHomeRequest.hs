{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Driver.GoHomeFeature.DriverGoHomeRequest where

import Data.Time (UTCTime (..))
import Database.Beam as B
import qualified Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import Domain.Types.Person
import qualified EulerHS.Language as L
import Kernel.Beam.Functions (findAllWithKV, findAllWithOptionsKV, findOneWithKV, getMasterBeamConfig, updateOneWithKV)
import Kernel.Prelude
import Kernel.Types.App
import Kernel.Types.Common
import Kernel.Types.Id as Id
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import Storage.Beam.Driver.GoHomeFeature.DriverGoHomeRequest as BeamDHR
import Storage.Queries.Driver.GoHomeFeature.DriverGoHomeRequest.Internal ()

create :: MonadFlow m => DDGR.DriverGoHomeRequest -> m ()
create newDriverGoHomeRequest = do
  dbConf <- getMasterBeamConfig
  void $
    L.runDB dbConf $
      L.insertRows $
        B.insert (BeamCommon.driverGoHomeRequest BeamCommon.atlasDB) $
          B.insertExpressions
            [BeamDHR.toRowExpression (newDriverGoHomeRequest.id.getId) (getId newDriverGoHomeRequest.driverId) newDriverGoHomeRequest.lat newDriverGoHomeRequest.lon newDriverGoHomeRequest.status newDriverGoHomeRequest.numCancellation newDriverGoHomeRequest.createdAt newDriverGoHomeRequest.updatedAt]

findById :: (MonadFlow m) => Id DDGR.DriverGoHomeRequest -> m (Maybe DDGR.DriverGoHomeRequest)
findById (Id.Id driverGoHomeRequestId) = findOneWithKV [Se.Is BeamDHR.id $ Se.Eq driverGoHomeRequestId]

findActive :: (MonadFlow m, Log m) => Id Driver -> m (Maybe DDGR.DriverGoHomeRequest)
findActive (Id.Id driverId) = findAllWithOptionsKV [Se.And [Se.Is BeamDHR.driverId $ Se.Eq driverId, Se.Is BeamDHR.status $ Se.Eq DDGR.ACTIVE]] (Se.Desc BeamDHR.createdAt) (Just 1) Nothing <&> listToMaybe

finishWithStatus :: MonadFlow m => Id DDGR.DriverGoHomeRequest -> DDGR.DriverGoHomeRequestStatus -> m ()
finishWithStatus (Id.Id driverGoHomeRequestId) status = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamDHR.status status, Se.Set BeamDHR.updatedAt now]
    [Se.Is BeamDHR.id $ Se.Eq driverGoHomeRequestId]

todaySuccessCount :: (MonadFlow m) => Id Driver -> m Int
todaySuccessCount driverId = do
  now <- getCurrentTime
  length <$> findAllWithKV [Se.Is BeamDHR.driverId $ Se.Eq $ getId driverId, Se.Is BeamDHR.status $ Se.Eq DDGR.SUCCESS, Se.Is BeamDHR.createdAt $ Se.GreaterThanOrEq now {utctDayTime = 0}, Se.Is BeamDHR.createdAt $ Se.LessThanOrEq now {utctDayTime = 86400}]

updateCancellationCount :: (MonadFlow m) => Id DDGR.DriverGoHomeRequest -> Int -> m ()
updateCancellationCount dghrId val = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamDHR.numCancellation val, Se.Set BeamDHR.updatedAt now]
    [Se.Is BeamDHR.id $ Se.Eq $ getId dghrId]
