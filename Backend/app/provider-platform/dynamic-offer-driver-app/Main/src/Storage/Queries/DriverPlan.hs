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
import qualified Domain.Types.DriverPlan as DPPS
import Domain.Types.Person
import Domain.Types.PlanDetails (PaymentMode, PlanDetails)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.DriverPlan

create :: DriverPlan -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id DriverPlan -> m (Maybe DriverPlan)
findById = Esq.findById

findByDriverId :: Transactionable m => Id Person -> m (Maybe DriverPlan)
findByDriverId driverId = do
  findOne $ do
    driverPlan <- from $ table @DriverPlanT
    where_ $
      driverPlan ^. DriverPlanDriverId ==. val (toKey driverId)
    return driverPlan

findByDriverIdAndPlanId :: Transactionable m => Id Person -> Id PlanDetails -> m (Maybe DriverPlan)
findByDriverIdAndPlanId driverId planId = do
  findOne $ do
    driverPlan <- from $ table @DriverPlanT
    where_ $
      driverPlan ^. DriverPlanDriverId ==. val (toKey driverId) &&. driverPlan ^. DriverPlanPlanId ==. val (toKey planId)
    return driverPlan

updateMandateStatusByDriverId :: DPPS.MandateStatus -> Id Person -> SqlDB ()
updateMandateStatusByDriverId mandateStatus driverId = do
  update $ \tbl -> do
    set
      tbl
      [ DriverPlanMandateStatus =. val (Just mandateStatus)
      ]
    where_ $ tbl ^. DriverPlanDriverId ==. val (toKey driverId)

updatePlanIdByDriverId :: Id Person -> Id PlanDetails -> SqlDB ()
updatePlanIdByDriverId driverId planId = do
  update $ \tbl -> do
    set
      tbl
      [DriverPlanPlanId =. val (toKey planId)]
    where_ $ tbl ^. DriverPlanDriverId ==. val (toKey driverId)

updatePaymentModeAndStatusByDriverId :: Id Person -> PaymentMode -> PlanStatus -> SqlDB ()
updatePaymentModeAndStatusByDriverId driverId paymentMode planStatus = do
  update $ \tbl -> do
    set
      tbl
      [ DriverPlanPlanType =. val paymentMode,
        DriverPlanPlanStatus =. val planStatus
      ]
    where_ $ tbl ^. DriverPlanDriverId ==. val (toKey driverId)
