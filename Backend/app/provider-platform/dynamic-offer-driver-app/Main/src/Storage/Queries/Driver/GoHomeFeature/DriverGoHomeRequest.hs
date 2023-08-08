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
--import Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest

-- import Kernel.Utils.Common

import Database.Beam as B
import qualified Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import Domain.Types.Person
import qualified EulerHS.Language as L
import Kernel.Beam.Functions (findAllWithKV, findAllWithOptionsKV, getLocationDbBeamConfig, getMasterBeamConfig, updateOneWithKV)
import Kernel.Prelude
import Kernel.Types.App
import Kernel.Types.Common
import Kernel.Types.Id as Id
import Lib.Utils (buildRadiusWithin'', byDist)
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import Storage.Beam.Driver.GoHomeFeature.DriverGoHomeRequest as BeamDHR
import Storage.Queries.Driver.GoHomeFeature.DriverGoHomeRequest.Internal ()
import Tools.Maps (LatLong (..))

--create :: DriverGoHomeRequest -> SqlDB ()
-- create newDriverGoHomeRequest = do
--   -- Tricky query to be able to insert meaningful Point
--   Esq.insertSelect $
--     return $
--       DriverGoHomeRequestT
--         <# val (newDriverGoHomeRequest.id.getId)
--         <#> val (toKey $ newDriverGoHomeRequest.driverId)
--         <#> val newDriverGoHomeRequest.lat
--         <#> val newDriverGoHomeRequest.lon
--         <#> Esq.getPoint (val newDriverGoHomeRequest.lat, val newDriverGoHomeRequest.lon)
--         <#> val newDriverGoHomeRequest.status
--         <#> val newDriverGoHomeRequest.createdAt
--         <#> val newDriverGoHomeRequest.updatedAt

create :: MonadFlow m => DDGR.DriverGoHomeRequest -> m ()
create newDriverGoHomeRequest = do
  --now <- getCurrentTime
  dbConf <- getLocationDbBeamConfig
  void $
    L.runDB dbConf $
      L.insertRows $
        B.insert (BeamCommon.driverGoHomeRequest BeamCommon.atlasDB) $
          B.insertExpressions
            [BeamDHR.toRowExpression (newDriverGoHomeRequest.id.getId) (getId newDriverGoHomeRequest.driverId) newDriverGoHomeRequest.lat newDriverGoHomeRequest.lon newDriverGoHomeRequest.status newDriverGoHomeRequest.createdAt newDriverGoHomeRequest.updatedAt]

-- findActive :: Transactionable m => Id Driver -> m (Maybe DriverGoHomeRequest)
-- findActive driverId = do
--   Esq.findOne $ do
--     driverGoHomeRequest <- from $ table @DriverGoHomeRequestT
--     where_ $
--       driverGoHomeRequest ^. DriverGoHomeRequestDriverId ==. val (toKey driverId)
--         &&. driverGoHomeRequest ^. DriverGoHomeRequestStatus ==. val DDGR.ACTIVE
--     Esq.limit 1
--     Esq.orderBy [Esq.desc $ driverGoHomeRequest ^. DriverGoHomeRequestCreatedAt]
--     return driverGoHomeRequest

findActive :: (MonadFlow m, Log m) => Id Driver -> m (Maybe DDGR.DriverGoHomeRequest)
findActive (Id.Id driverId) = findAllWithOptionsKV [Se.And [Se.Is BeamDHR.driverId $ Se.Eq driverId, Se.Is BeamDHR.status $ Se.Eq DDGR.ACTIVE]] (Se.Desc BeamDHR.createdAt) (Just 1) Nothing <&> listToMaybe

-- finishSuccessfully :: Id DriverGoHomeRequest -> SqlDB ()
-- finishSuccessfully driverGoHomeRequestId = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverGoHomeRequestStatus =. val DDGR.SUCCESS,
--         DriverGoHomeRequestUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. DriverGoHomeRequestTId ==. val (toKey driverGoHomeRequestId)

finishSuccessfully :: MonadFlow m => Id DDGR.DriverGoHomeRequest -> m ()
finishSuccessfully (Id.Id driverGoHomeRequestId) = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamDHR.status DDGR.SUCCESS, Se.Set BeamDHR.updatedAt now]
    [Se.Is BeamDHR.id $ Se.Eq driverGoHomeRequestId]

-- finishActiveFailed :: Id Driver -> SqlDB ()
-- finishActiveFailed driverId = do
--   mbActive <- findActive driverId
--   whenJust mbActive $ \active -> do
--     now <- getCurrentTime
--     Esq.update $ \tbl -> do
--       set
--         tbl
--         [ DriverGoHomeRequestStatus =. val DDGR.FAILED,
--           DriverGoHomeRequestUpdatedAt =. val now
--         ]
--       where_ $ tbl ^. DriverGoHomeRequestTId ==. val (toKey $ active.id)

finishActiveFailed :: (MonadFlow m) => Id Driver -> m ()
finishActiveFailed driverId = do
  mbActive <- findActive driverId
  whenJust mbActive $ \active -> do
    now <- getCurrentTime
    updateOneWithKV
      [Se.Set BeamDHR.status DDGR.FAILED, Se.Set BeamDHR.updatedAt now]
      [Se.Is BeamDHR.id $ Se.Eq $ getId active.id]

finishWithFailure :: (MonadFlow m) => Id DDGR.DriverGoHomeRequest -> m ()
finishWithFailure ghrId = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamDHR.status DDGR.FAILED, Se.Set BeamDHR.updatedAt now]
    [Se.Is BeamDHR.id $ Se.Eq $ getId ghrId]

-- isWithinRange ::
--   (Transactionable m) =>
--   Id DriverGoHomeRequest ->
--   LatLong ->
--   Meters ->
--   m Bool
-- isWithinRange driverGoHomeRequestId LatLong {..} radiusMeters = do
--   res <- Esq.findOne $ do
--     goHomeReq <- from $ table @DriverGoHomeRequestT
--     where_ $ goHomeReq ^. DriverGoHomeRequestTId ==. val (toKey driverGoHomeRequestId)
--     orderBy [asc (goHomeReq ^. DriverGoHomeRequestPoint <->. Esq.getPoint (val lat, val lon))]
--     return $ buildRadiusWithin @Bool (goHomeReq ^. DriverGoHomeRequestPoint) (lat, lon) (val radiusMeters.getMeters)
--   return $ fromMaybe False res

isWithinRange :: (MonadFlow m) => Id DDGR.DriverGoHomeRequest -> LatLong -> Meters -> m Bool
isWithinRange (Id.Id driverGoHomeRequestId') LatLong {..} radiusMeters = do
  dbConf <- getMasterBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.orderBy_ (\_ -> B.asc_ (byDist (lat, lon))) $
          B.limit_ 1 $
            B.filter_'
              (\dhr -> BeamDHR.id dhr B.==?. B.val_ driverGoHomeRequestId' B.&&?. buildRadiusWithin'' (lat, lon) (getMeters radiusMeters))
              do
                B.all_ (BeamCommon.driverGoHomeRequest BeamCommon.atlasDB)
  either (const (pure False)) (\r -> if null r then pure False else pure True) res

-- todaySuccessCount ::
--   (Transactionable m, MonadTime m) =>
--   Id Driver ->
--   m Int
-- todaySuccessCount driverId = do
--   now <- getCurrentTime
--   res <- Esq.findOne $ do
--     goHomeReq <- from $ table @DriverGoHomeRequestT
--     where_ $
--       goHomeReq ^. DriverGoHomeRequestDriverId ==. val (toKey driverId)
--         &&. goHomeReq ^. DriverGoHomeRequestCreatedAt >=. val now {utctDayTime = 0}
--         &&. goHomeReq ^. DriverGoHomeRequestCreatedAt <=. val now {utctDayTime = 86400}
--         &&. goHomeReq ^. DriverGoHomeRequestStatus ==. val DDGR.SUCCESS
--     return (Esq.count @Int $ goHomeReq ^. DriverGoHomeRequestTId)
--   return $ fromMaybe 0 res

todaySuccessCount :: (MonadFlow m) => Id Driver -> m Int
todaySuccessCount driverId = do
  now <- getCurrentTime
  length <$> findAllWithKV [Se.Is BeamDHR.driverId $ Se.Eq $ getId driverId, Se.Is BeamDHR.status $ Se.Eq DDGR.SUCCESS, Se.Is BeamDHR.createdAt $ Se.GreaterThanOrEq now {utctDayTime = 0}, Se.Is BeamDHR.createdAt $ Se.LessThanOrEq now {utctDayTime = 86400}]
