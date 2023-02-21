module Storage.Queries.Person.PersonFlowStatus where

import Domain.Types.Person
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
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

deleteByPersonId :: Id Person -> SqlDB ()
deleteByPersonId personId = do
  Esq.delete $ do
    personFlowStatus <- from $ table @PersonFlowStatusT
    where_ (personFlowStatus ^. PersonFlowStatusTId ==. val (toKey personId))

updateToIdleMultiple :: [Id Person] -> UTCTime -> SqlDB ()
updateToIdleMultiple personIds now = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonFlowStatusUpdatedAt =. val now,
        PersonFlowStatusFlowStatus =. val DPFS.IDLE
      ]
    where_ $ tbl ^. PersonFlowStatusTId `in_` valList (toKey <$> personIds)
