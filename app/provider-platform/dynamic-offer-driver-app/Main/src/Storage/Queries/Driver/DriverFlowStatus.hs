module Storage.Queries.Driver.DriverFlowStatus where

import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import Domain.Types.Person
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.Driver.DriverFlowStatus

create :: DDFS.DriverFlowStatus -> SqlDB ()
create = Esq.create

deleteById :: Id Person -> SqlDB ()
deleteById = Esq.deleteByKey @DriverFlowStatusT

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
