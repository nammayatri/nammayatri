{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Person.PersonFlowStatus where

import Domain.Types.Person
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.Person.PersonFlowStatus

create :: DPFS.PersonFlowStatus -> SqlDB m ()
create = Esq.create

getStatus ::
  forall m ma.
  Transactionable ma m =>
  Id Person ->
  Proxy ma ->
  m (Maybe DPFS.FlowStatus)
getStatus personId _ = do
  findOne @m @ma $ do
    personFlowStatus <- from $ table @PersonFlowStatusT
    where_ $
      personFlowStatus ^. PersonFlowStatusTId ==. val (toKey personId)
    return $ personFlowStatus ^. PersonFlowStatusFlowStatus

updateStatus :: Id Person -> DPFS.FlowStatus -> SqlDB m ()
updateStatus personId flowStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonFlowStatusUpdatedAt =. val now,
        PersonFlowStatusFlowStatus =. val flowStatus
      ]
    where_ $ tbl ^. PersonFlowStatusTId ==. val (toKey personId)

deleteByPersonId :: Id Person -> SqlDB m ()
deleteByPersonId personId = do
  Esq.delete $ do
    personFlowStatus <- from $ table @PersonFlowStatusT
    where_ (personFlowStatus ^. PersonFlowStatusTId ==. val (toKey personId))

updateToIdleMultiple :: [Id Person] -> UTCTime -> SqlDB m ()
updateToIdleMultiple personIds now = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonFlowStatusUpdatedAt =. val now,
        PersonFlowStatusFlowStatus =. val DPFS.IDLE
      ]
    where_ $ tbl ^. PersonFlowStatusTId `in_` valList (toKey <$> personIds)
