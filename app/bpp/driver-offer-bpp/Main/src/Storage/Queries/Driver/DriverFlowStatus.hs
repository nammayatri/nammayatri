module Storage.Queries.Driver.DriverFlowStatus where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import Domain.Types.Person
import Storage.Tabular.Driver.DriverFlowStatus

create :: DDFS.DriverFlowStatus -> SqlDB ()
create = Esq.create

getStatus ::
  (Transactionable m) =>
  Id Person ->
  m (Maybe DDFS.FlowStatus)
getStatus personId = do
  findOne $ do
    driverFlowStatus <- from $ table @DriverFlowStatusT
    where_ $
      driverFlowStatus ^. DriverFlowStatusTId ==. val (toKey personId)
    return $ driverFlowStatus ^. DriverFlowStatusFlowStatus

updateStatus :: Id Person -> DDFS.FlowStatus -> SqlDB ()
updateStatus personId flowStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverFlowStatusUpdatedAt =. val now,
        DriverFlowStatusFlowStatus =. val flowStatus
      ]
    where_ $ tbl ^. DriverFlowStatusTId ==. val (toKey personId)
