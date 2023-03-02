{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Customer
  ( deleteCustomer,
  )
where

import qualified "dashboard-helper-api" Dashboard.Common as Common
import qualified Domain.Types.Booking.Type as DRB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Transactionable (Transactionable' (runTransaction), runInReplica)
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.SavedReqLocation as QSRL

deleteCustomer ::
  ShortId DM.Merchant ->
  Id Common.Customer ->
  Flow APISuccess
deleteCustomer merchantShortId customerId = do
  let personId = cast @Common.Customer @DP.Person customerId
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  person <- runInReplica $ QP.findById personId (Proxy @Flow) >>= fromMaybeM (PersonNotFound $ getId personId)
  unless (merchant.id == person.merchantId) $ throwError (PersonDoesNotExist $ getId personId)
  bookings <- runInReplica $ QRB.findByRiderIdAndStatus personId [DRB.NEW, DRB.TRIP_ASSIGNED, DRB.AWAITING_REASSIGNMENT, DRB.CONFIRMED, DRB.COMPLETED] (Proxy @Flow)
  unless (null bookings) $ throwError (InvalidRequest "Can't delete customer, has a valid booking in past.")
  runTransaction $ do
    QPFS.deleteByPersonId @Flow personId
    QSRL.deleteAllByRiderId personId
    QP.deleteById personId
  pure Success
