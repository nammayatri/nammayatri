module Storage.Queries.Person.PersonFlowStatus where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Person
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import Storage.Tabular.Person.PersonFlowStatus

create :: DPFS.PersonFlowStatus -> SqlDB ()
create = Esq.create

getStatus ::
  (Transactionable m) =>
  Id Person ->
  m (Maybe DPFS.FlowStatus)
getStatus personId = do
  findOne $ do
    personFlowStatus <- from $ table @PersonFlowStatusT
    where_ $
      personFlowStatus ^. PersonFlowStatusTId ==. val (toKey personId)
    return $ personFlowStatus ^. PersonFlowStatusFlowStatus

updateStatus :: Id Person -> DPFS.FlowStatus -> SqlDB ()
updateStatus personId flowStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonFlowStatusUpdatedAt =. val now,
        PersonFlowStatusFlowStatus =. val flowStatus
      ]
    where_ $ tbl ^. PersonFlowStatusTId ==. val (toKey personId)
