{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Driver.DriverFlowStatus where

import Domain.Types.Driver.DriverFlowStatus (isPaymentOverdue)
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

clearPaymentStatus :: Id Person -> Bool -> SqlDB ()
clearPaymentStatus personId isActive = do
  let status = if isActive then DDFS.ACTIVE else DDFS.IDLE
  updateStatus' False personId status

updateStatus :: Id Person -> DDFS.FlowStatus -> SqlDB ()
updateStatus = updateStatus' True

updateStatus' :: Bool -> Id Person -> DDFS.FlowStatus -> SqlDB ()
updateStatus' checkForPayment personId flowStatus = do
  driverStatus <- getStatus personId
  case driverStatus of
    Nothing -> pure ()
    Just ds -> when (not checkForPayment || not (isPaymentOverdue ds)) $ do
      now <- getCurrentTime
      Esq.update $ \tbl -> do
        set
          tbl
          [ DriverFlowStatusUpdatedAt =. val now,
            DriverFlowStatusFlowStatus =. val flowStatus
          ]
        where_ $ tbl ^. DriverFlowStatusTId ==. val (toKey personId)
