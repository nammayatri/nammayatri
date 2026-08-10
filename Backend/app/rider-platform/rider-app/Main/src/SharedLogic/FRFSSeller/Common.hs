{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FRFSSeller.Common
  ( sellerRiderId,
    isSellerRider,
  )
where

import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Id

-- | Synthetic rider identity for bookings this app SELLS rather than buys.
--
-- A seller has no 'DP.Person': the passenger belongs to the buyer app, and all we
-- ever receive is a phone number on @init@. Reusing 'frfs_ticket_booking' therefore
-- needs a stand-in for its non-null @riderId@.
--
-- This mirrors 'SharedLogic.FRFSUtils.partnerOrgRiderId', which has served exactly
-- this purpose in production for partner-org bookings that likewise have no rider.
--
-- Safety: both places that read @riderId@ for authorization fail closed against a
-- sentinel. Access checks compare it to the logged-in @personId@ (never equal, so
-- the request is denied), and history queries filter BY @riderId@ (so seller rows
-- never surface in a real rider's history).
--
-- Not yet referenced: nothing in Phase 1 persists a booking. The first caller
-- arrives with @confirm@ in Phase 2. It lives here now because it is the single
-- decision the table-reuse strategy rests on, and it belongs somewhere reviewable
-- rather than inlined at a future call site.
sellerRiderId :: Id DP.Person
sellerRiderId = Id "frfsSeller_rider_id"

-- | True when a booking was sold by us rather than bought on a rider's behalf.
isSellerRider :: Id DP.Person -> Bool
isSellerRider = (== sellerRiderId)
