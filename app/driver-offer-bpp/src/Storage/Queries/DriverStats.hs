module Storage.Queries.DriverStats where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.DriverStats
import Storage.Tabular.DriverStats
import Types.App
import Utils.Common

createInitialDriverStats :: Id Driver -> SqlDB ()
createInitialDriverStats driverId = do
  now <- getCurrentTime
  create' $
    DriverStats
      { driverId = driverId,
        idleSince = now
      }

getTopDriversByIdleTime :: Transactionable m => Int -> [Id Driver] -> m [Id Driver]
getTopDriversByIdleTime count_ ids =
  Esq.findAll $ do
    driverStats <- from $ table @DriverStatsT
    where_ $ driverStats ^. DriverStatsDriverId `in_` valList (toKey . cast <$> ids)
    orderBy [asc $ driverStats ^. DriverStatsIdleSince]
    limit $ fromIntegral count_
    return $ driverStats ^. DriverStatsTId

updateIdleTime :: Id Driver -> SqlDB ()
updateIdleTime driverId = updateIdleTimes [driverId]

updateIdleTimes :: [Id Driver] -> SqlDB ()
updateIdleTimes driverIds = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ DriverStatsIdleSince =. val now
      ]
    where_ $ tbl ^. DriverStatsDriverId `in_` valList (toKey . cast <$> driverIds)

fetchAll :: Transactionable m => m [DriverStats]
fetchAll = Esq.findAll $ from $ table @DriverStatsT

deleteById :: Id Driver -> SqlDB ()
deleteById = deleteByKey' @DriverStatsT
