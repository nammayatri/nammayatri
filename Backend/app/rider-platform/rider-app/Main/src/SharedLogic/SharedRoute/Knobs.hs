-- | Demand-side knob dispatch for shared-route services (shuttle, shared cab, works bus on the one spine).
--
-- Every @case@ on a demand knob lives here, so a new product is a config row and not a branch somewhere in
-- a flow. An arm that no live product needs yet refuses with 'notImplemented' and names the module that
-- owns it; the shared cab's existing behaviour sits behind the arms it exercises today (CAPACITY seats,
-- LIVE allocation) rather than being copied.
module SharedLogic.SharedRoute.Knobs
  ( demandKnobsOf,
    requireDemandKnobs,
    notImplemented,
    holdSeats,
    BoardingEvidence (..),
    BoardingCheck (..),
    verifyBoarding,
    allocate,
    CancelVerdict (..),
    cancelPolicy,
  )
where

import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.SharedRoute
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.SharedCab.Allocation as Allocation
import SharedLogic.SharedCab.Allocation.Claim (AllocFlow)
import qualified SharedLogic.SharedCab.Seats as Seats
import SharedLogic.SharedCab.SessionState (Session)

-- | The feed's demand knobs, Nothing for an operator feed (metro, government bus) that has none.
demandKnobsOf :: DIBC.IntegratedBPPConfig -> Maybe SharedDemandKnobs
demandKnobsOf config = case config.providerConfig of
  DIBC.DIRECT direct -> direct.sharedKnobs
  _ -> Nothing

-- | The feed's demand knobs, validated. A feed on the shared-route spine without knobs, or with an
-- impossible pair, is a config error and fails here rather than somewhere down the flow.
requireDemandKnobs :: MonadFlow m => DIBC.IntegratedBPPConfig -> m SharedDemandKnobs
requireDemandKnobs config = do
  knobs <- demandKnobsOf config & fromMaybeM (InvalidRequest $ "No shared knobs on integrated bpp config " <> config.id.getId)
  either (throwError . InternalError . (("Invalid shared knobs on " <> config.id.getId <> ": ") <>)) pure (validateDemandKnobs knobs)
  pure knobs

-- | A knob value that exists in the model but that no product has needed yet. Say which module owns it.
notImplemented :: MonadFlow m => Text -> m a
notImplemented what = throwError $ InternalError $ "shared route: " <> what <> " is not implemented yet"

-- | Hold the passenger's seats on the vehicle for their stop range. True when they fit.
holdSeats :: Seats.SeatFlow m r => SharedDemandKnobs -> Session -> Id DFTB.FRFSTicketBooking -> Text -> Text -> Int -> m Bool
holdSeats knobs session bookingId boardStop dropStop party = case knobs.seats of
  UNLIMITED -> pure True
  CAPACITY -> Seats.takeSeats session bookingId boardStop dropStop party
  RESERVED -> notImplemented "RESERVED seats: numbered seats per trip through FRFSSeatBooking (module S)"
  MIXED -> notImplemented "MIXED seats: a reserved block plus capacity (module S)"

-- | What the boarding request carried.
data BoardingEvidence
  = NoEvidence
  | -- | the rider typed the code printed in the vehicle
    TypedCode Text
  | -- | the driver entered the rider's per-booking OTP
    PresentedOtp Text
  | -- | a conductor scanned the ticket
    ConductorScanned
  | -- | the rider entered the vehicle number
    CheckedInOnPlate Text
  deriving (Show, Eq)

-- | What the caller must now check, or do.
data BoardingCheck
  = -- | board them, nothing to verify
    Admit
  | -- | compare against the booking's OTP
    CheckOtp Text
  | -- | compare against the vehicle's sticker code
    CheckStickerCode Text
  | -- | compare against the allocated vehicle
    CheckPlate Text
  | -- | refuse, with the reason to show
    RejectBoarding Text
  deriving (Show, Eq)

-- | Pure: which check the boarding knob asks for, given what the request carried.
verifyBoarding :: BoardingMode -> BoardingEvidence -> BoardingCheck
verifyBoarding mode evidence = case (mode, evidence) of
  (NO_CHECK, _) -> Admit
  (GATE, _) -> RejectBoarding "boarding is validated at the operator's gate, not in the app"
  (CONDUCTOR_SCAN, ConductorScanned) -> Admit
  (CONDUCTOR_SCAN, _) -> RejectBoarding "a conductor must scan the ticket"
  (PLATE_CHECKIN, CheckedInOnPlate plate) -> CheckPlate plate
  (PLATE_CHECKIN, _) -> RejectBoarding "the rider must check in with the vehicle number"
  (STICKER_CODE, TypedCode code) -> CheckStickerCode code
  (STICKER_CODE, _) -> RejectBoarding "the rider must type the code shown in the vehicle"
  (OTP, PresentedOtp otp) -> CheckOtp otp
  (OTP, _) -> RejectBoarding "the driver must enter the rider's boarding OTP"

-- | Bind waiting passengers of a city to vehicles, as the allocation knob says.
allocate :: AllocFlow m r => SharedDemandKnobs -> Id DMOC.MerchantOperatingCity -> m ()
allocate knobs cityId = case knobs.allocation of
  -- the rider boards whatever comes; nothing to bind
  NO_ALLOCATION -> pure ()
  -- the ticket is already on one run; the driver platform staffs that run when it materialises it
  PRE_ASSIGNED -> pure ()
  -- the shared cab's live allocation tick, unchanged
  LIVE -> Allocation.triggerSharedCabAllocation cityId

data CancelVerdict
  = -- | before the cutoff: the trip or fare comes back in full
    CancelFree
  | -- | after the cutoff, before departure: cancelling costs what a no-show would
    CancelLate NoShowConsequence
  | -- | at or after departure: too late to cancel
    CancelClosed
  deriving (Show, Eq)

-- | Pure: what cancelling now, for a run leaving at @departure@, costs the rider. The shared cab (LIVE
-- allocation, no departure time) keeps its own SharedLogic.SharedCab.RefundPolicy until it moves onto runs.
cancelPolicy :: SharedDemandKnobs -> UTCTime -> UTCTime -> CancelVerdict
cancelPolicy knobs now departure
  | now >= departure = CancelClosed
  | otherwise = case knobs.cancelCutoffMinutes of
    Nothing -> CancelFree
    Just cutoff
      | now <= addUTCTime (negate (fromIntegral cutoff * 60)) departure -> CancelFree
      | otherwise -> CancelLate knobs.noShowConsequence
