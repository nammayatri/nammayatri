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
  person <- runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound $ getId personId)
  unless (merchant.id == person.merchantId) $ throwError (PersonDoesNotExist $ getId personId)
  bookings <- runInReplica $ QRB.findByRiderIdAndStatus personId [DRB.NEW, DRB.TRIP_ASSIGNED, DRB.AWAITING_REASSIGNMENT, DRB.CONFIRMED, DRB.COMPLETED]
  unless (null bookings) $ throwError (InvalidRequest "Can't delete customer, has a valid booking in past.")
  runTransaction $ do
    QPFS.deleteByPersonId personId
    QSRL.deleteAllByRiderId personId
    QP.deleteById personId
  pure Success
