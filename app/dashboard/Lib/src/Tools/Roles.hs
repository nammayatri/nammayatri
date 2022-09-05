module Tools.Roles where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Person as DP
import Tools.Servant.HeaderAuth

data USER

data ADMIN

data JUSPAY_OPS

-- We can use for api more complex payloads with different type level constructors, like this:
-- type UpdateBooking =
--   "updateBooking"
--     :> AccessLevel (WriteAccess [Rides, Bookings])
--     :> Post '[JSON] UpdateBookingReq

instance (VerificationPayload Role) USER where
  toPayloadType _ = USER

instance (VerificationPayload Role) ADMIN where
  toPayloadType _ = ADMIN

instance (VerificationPayload Role) JUSPAY_OPS where
  toPayloadType _ = JUSPAY_OPS

verifyRole :: EsqDBFlow m r => DP.Role -> Id DP.Person -> m (Id DP.Person)
verifyRole _pr _personId = error "TODO"
