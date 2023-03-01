{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Driver.DriverFlowStatus where

import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import Domain.Types.Person
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.Driver.DriverFlowStatus

create :: DDFS.DriverFlowStatus -> SqlDB m ()
create = Esq.create

deleteById :: Id Person -> SqlDB m ()
deleteById = Esq.deleteByKey @DriverFlowStatusT

getStatus ::
  forall ma m.
  (Transactionable ma m) =>
  Id Person ->
  Proxy ma ->
  m (Maybe DDFS.FlowStatus)
getStatus personId _ = do
  findOne @m @ma $ do
    driverFlowStatus <- from $ table @DriverFlowStatusT
    where_ $
      driverFlowStatus ^. DriverFlowStatusTId ==. val (toKey personId)
    return $ driverFlowStatus ^. DriverFlowStatusFlowStatus

updateStatus :: Id Person -> DDFS.FlowStatus -> SqlDB m ()
updateStatus personId flowStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverFlowStatusUpdatedAt =. val now,
        DriverFlowStatusFlowStatus =. val flowStatus
      ]
    where_ $ tbl ^. DriverFlowStatusTId ==. val (toKey personId)
