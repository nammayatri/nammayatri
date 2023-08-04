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
import Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest
import qualified Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import Domain.Types.Person
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.Driver.GoHomeFeature.DriverGoHomeRequest
import Tools.Maps (LatLong (..))

create :: DriverGoHomeRequest -> SqlDB ()
create newDriverGoHomeRequest = do
  -- Tricky query to be able to insert meaningful Point
  Esq.insertSelect $
    return $
      DriverGoHomeRequestT
        <# val (newDriverGoHomeRequest.id.getId)
        <#> val (toKey $ newDriverGoHomeRequest.driverId)
        <#> val newDriverGoHomeRequest.lat
        <#> val newDriverGoHomeRequest.lon
        <#> Esq.getPoint (val newDriverGoHomeRequest.lat, val newDriverGoHomeRequest.lon)
        <#> val newDriverGoHomeRequest.status
        <#> val newDriverGoHomeRequest.createdAt
        <#> val newDriverGoHomeRequest.updatedAt

findActive :: Transactionable m => Id Driver -> m (Maybe DriverGoHomeRequest)
findActive driverId = do
  Esq.findOne $ do
    driverGoHomeRequest <- from $ table @DriverGoHomeRequestT
    where_ $
      driverGoHomeRequest ^. DriverGoHomeRequestDriverId ==. val (toKey driverId)
        &&. driverGoHomeRequest ^. DriverGoHomeRequestStatus ==. val DDGR.ACTIVE
    Esq.limit 1
    Esq.orderBy [Esq.desc $ driverGoHomeRequest ^. DriverGoHomeRequestCreatedAt]
    return driverGoHomeRequest

finishSuccessfully :: Id DriverGoHomeRequest -> SqlDB ()
finishSuccessfully driverGoHomeRequestId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverGoHomeRequestStatus =. val DDGR.SUCCESS,
        DriverGoHomeRequestUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverGoHomeRequestTId ==. val (toKey driverGoHomeRequestId)

finishActiveFailed :: Id Driver -> SqlDB ()
finishActiveFailed driverId = do
  mbActive <- findActive driverId
  whenJust mbActive $ \active -> do
    now <- getCurrentTime
    Esq.update $ \tbl -> do
      set
        tbl
        [ DriverGoHomeRequestStatus =. val DDGR.FAILED,
          DriverGoHomeRequestUpdatedAt =. val now
        ]
      where_ $ tbl ^. DriverGoHomeRequestTId ==. val (toKey $ active.id)

isWithinRange ::
  (Transactionable m) =>
  Id DriverGoHomeRequest ->
  LatLong ->
  Meters ->
  m Bool
isWithinRange driverGoHomeRequestId LatLong {..} radiusMeters = do
  res <- Esq.findOne $ do
    goHomeReq <- from $ table @DriverGoHomeRequestT
    where_ $ goHomeReq ^. DriverGoHomeRequestTId ==. val (toKey driverGoHomeRequestId)
    orderBy [asc (goHomeReq ^. DriverGoHomeRequestPoint <->. Esq.getPoint (val lat, val lon))]
    return $ buildRadiusWithin @Bool (goHomeReq ^. DriverGoHomeRequestPoint) (lat, lon) (val radiusMeters.getMeters)
  return $ fromMaybe False res

todaySuccessCount ::
  (Transactionable m, MonadTime m) =>
  Id Driver ->
  m Int
todaySuccessCount driverId = do
  now <- getCurrentTime
  res <- Esq.findOne $ do
    goHomeReq <- from $ table @DriverGoHomeRequestT
    where_ $
      goHomeReq ^. DriverGoHomeRequestDriverId ==. val (toKey driverId)
        &&. goHomeReq ^. DriverGoHomeRequestCreatedAt >=. val now {utctDayTime = 0}
        &&. goHomeReq ^. DriverGoHomeRequestCreatedAt <=. val now {utctDayTime = 86400}
        &&. goHomeReq ^. DriverGoHomeRequestStatus ==. val DDGR.SUCCESS
    return (Esq.count @Int $ goHomeReq ^. DriverGoHomeRequestTId)
  return $ fromMaybe 0 res
