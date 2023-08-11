{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverPlan where

import Domain.Types.DriverPlan
import qualified Domain.Types.Mandate as DM
import Domain.Types.Person
import Domain.Types.Plan
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.DriverPlan

create :: DriverPlan -> SqlDB ()
create = Esq.create

findByDriverId :: Transactionable m => Id Person -> m (Maybe DriverPlan)
findByDriverId driverId = do
  findOne $ do
    driverPlan <- from $ table @DriverPlanT
    where_ $
      driverPlan ^. DriverPlanDriverId ==. val (toKey driverId)
    return driverPlan

findByMandateId :: Transactionable m => Id DM.Mandate -> m (Maybe DriverPlan)
findByMandateId mandateId = do
  findOne $ do
    driverPlan <- from $ table @DriverPlanT
    where_ $
      driverPlan ^. DriverPlanMandateId ==. val (Just $ toKey mandateId)
    return driverPlan

updatePlanIdByDriverId :: Id Person -> Id Plan -> SqlDB ()
updatePlanIdByDriverId driverId planId = do
  now <- getCurrentTime
  update $ \tbl -> do
    set
      tbl
      [ DriverPlanPlanId =. val (toKey planId),
        DriverPlanUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverPlanDriverId ==. val (toKey driverId)

updateMandateIdByDriverId :: Id Person -> Id DM.Mandate -> SqlDB ()
updateMandateIdByDriverId driverId mandateId = do
  now <- getCurrentTime
  update $ \tbl -> do
    set
      tbl
      [ DriverPlanMandateId =. val (Just $ toKey mandateId),
        DriverPlanUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverPlanDriverId ==. val (toKey driverId)

updatePaymentModeByDriverId :: Id Person -> PaymentMode -> SqlDB ()
updatePaymentModeByDriverId driverId paymentMode = do
  now <- getCurrentTime
  update $ \tbl -> do
    set
      tbl
      [ DriverPlanPlanType =. val paymentMode,
        DriverPlanUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverPlanDriverId ==. val (toKey driverId)
