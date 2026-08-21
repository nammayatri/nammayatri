{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | MSIL's rider-selectable cancellation reasons.
--
-- The requirement defines ten reasons whose chargeability depends on the reason and on whether
-- the driver had arrived. ONDC's buyer-side enum covers five of them:
--
-- @
--   006  Safety concern with driver/ride        -> Safety or conduct concern
--   007  Vehicle appears unsafe or non-compliant-> Vehicle appears unsafe
--   003  Driver asked to cancel the ride        -> Driver reported a breakdown
--   004  My pickup location is incorrect        -> Incorrect booking details
--   005  Customer booked the ride mistakenly    -> Duplicate booking
-- @
--
-- The five below have no ONDC equivalent — including "found another ride" and "other reason",
-- which charge in both stages and are therefore the two that matter most.
--
-- ⚠️ PROVISIONAL WIRE CODES. The 9xxx values are ours, not ONDC's, and exist so the fee matrix
-- can be built and tested before ONDC publishes real ones. They sit far outside ONDC's
-- @000@–@017@ range so a stray placeholder in production logs is unmistakable.
--
-- When ONDC assigns real codes, change only 'riderReasonWireCode' — or move the reason into
-- 'Enums.CancellationReasonId' and drop it from here. The internal @RIDER_CANCEL_*@ codes are
-- what the fee rules key on and must not move: those rules are live JsonLogic with a rollout,
-- so repointing them is a production pricing change rather than a protocol one.
module SharedLogic.RiderCancellationReason
  ( RiderCancellationReason (..),
    riderReasonWireCode,
    riderReasonInternalCode,
    parseRiderCancellationReasonId,
  )
where

import qualified Data.Text as T
import EulerHS.Prelude

data RiderCancellationReason
  = MedicalEmergencyOrAccident
  | UnexpectedEventPreventsRide
  | RideNoLongerRequired
  | FoundAnotherRide
  | OtherReason
  deriving (Eq, Show, Generic, Bounded, Enum)

-- | ⚠️ Provisional. Replace with ONDC's values when published; nothing else needs to change.
riderReasonWireCode :: RiderCancellationReason -> Text
riderReasonWireCode = \case
  MedicalEmergencyOrAccident -> "9004"
  UnexpectedEventPreventsRide -> "9005"
  RideNoLongerRequired -> "9008"
  FoundAnotherRide -> "9009"
  OtherReason -> "9010"

-- | Stable. The fee matrix keys on these; they outlive whatever arrives on the wire.
riderReasonInternalCode :: RiderCancellationReason -> Text
riderReasonInternalCode = \case
  MedicalEmergencyOrAccident -> "RIDER_CANCEL_MEDICAL_EMERGENCY"
  UnexpectedEventPreventsRide -> "RIDER_CANCEL_UNEXPECTED_EVENT"
  RideNoLongerRequired -> "RIDER_CANCEL_NO_LONGER_REQUIRED"
  FoundAnotherRide -> "RIDER_CANCEL_FOUND_ANOTHER_RIDE"
  OtherReason -> "RIDER_CANCEL_OTHER"

-- | Built from @[minBound .. maxBound]@ so the parser cannot drift from the code table.
parseRiderCancellationReasonId :: Text -> Maybe RiderCancellationReason
parseRiderCancellationReasonId raw =
  let normalised = T.strip raw
   in find (\reason -> riderReasonWireCode reason == normalised) [minBound .. maxBound]
