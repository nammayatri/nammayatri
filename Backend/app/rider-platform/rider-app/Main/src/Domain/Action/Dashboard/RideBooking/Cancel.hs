module Domain.Action.Dashboard.RideBooking.Cancel (postCancelBooking) where

import qualified Beckn.ACL.Cancel as ACL
import qualified "this" Domain.Action.UI.Cancel
import qualified "this" Domain.Types.Booking
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QR
import Tools.Error

postCancelBooking ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Booking.Booking ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Domain.Action.UI.Cancel.CancelReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postCancelBooking merchantShortId _opCity bookingId personId req = withPersonIdLogTag personId $ do
  m <- findMerchantByShortId merchantShortId
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  mRide <- B.runInReplica $ QR.findActiveByRBId booking.id
  dCancelRes <- Domain.Action.UI.Cancel.cancel booking mRide req SBCR.ByMerchant
  void $ withShortRetry $ CallBPP.cancelV2 m.id dCancelRes.bppUrl =<< ACL.buildCancelReqV2 dCancelRes Nothing
  return Kernel.Types.APISuccess.Success
