-- | Pure rules of the shuttle subscription, kept apart from IO so they can be unit-tested
-- (hunit-tests/src/SharedCabPassRulesTests.hs).
module SharedLogic.SharedCabPassRules
  ( changeCutoff,
    otpLeadTime,
    otpLastCall,
    maxDaysPerRequest,
    maxReasonLength,
    localDay,
    localMidnightUtc,
    departureAt,
    issueTimeOn,
    inIssueWindow,
    otpDue,
    dayOpenForChange,
    liveTicketWindow,
    dayTicketWindow,
    isLiveStatus,
    addSkipDays,
    removeSkipDays,
    formatOtp,
    otpMatches,
    servesInOrder,
    slotOn,
    slotFor,
    setSlotChange,
    ridesSlotOn,
    ridesSlot,
    hasFreeSeat,
    rescheduleOpen,
  )
where

import Data.List (nub)
import qualified Data.Text as T
import Data.Time (addUTCTime)
import qualified Data.Time as DT
import qualified Domain.Types.FRFSTicketBookingStatus as DStatus
import qualified Domain.Types.PurchasedPassPayment as DPPP
import Kernel.Prelude
import Kernel.Types.Common (Seconds (..))
import Text.Printf (printf)

-- | Reschedule, skip and cancel close this long before the slot departs; the day's ticket is issued then.
changeCutoff :: NominalDiffTime
changeCutoff = 5 * 3600

-- | The boarding OTP, with the driver and vehicle, is sent this long before departure.
otpLeadTime :: NominalDiffTime
otpLeadTime = 3600

-- | Past this point before departure the OTP is sent even without a vehicle.
otpLastCall :: NominalDiffTime
otpLastCall = 900

-- | A monthly term has at most 31 days; two terms (current and renewal) at most 62.
maxDaysPerRequest :: Int
maxDaysPerRequest = 62

maxReasonLength :: Int
maxReasonLength = 200

localDay :: Seconds -> UTCTime -> DT.Day
localDay timeDiff t = DT.utctDay $ addUTCTime (fromIntegral timeDiff.getSeconds) t

localMidnightUtc :: Seconds -> DT.Day -> UTCTime
localMidnightUtc timeDiff day = addUTCTime (negate $ fromIntegral timeDiff.getSeconds) (DT.UTCTime day 0)

-- | GTFS departure: seconds after local midnight of the service day (may exceed 24h for a late trip).
departureAt :: Seconds -> DT.Day -> Int -> UTCTime
departureAt timeDiff day secondsAfterMidnight = addUTCTime (fromIntegral secondsAfterMidnight) (localMidnightUtc timeDiff day)

issueTimeOn :: UTCTime -> UTCTime
issueTimeOn = addUTCTime (negate changeCutoff)

-- | The job issues the day's ticket from the change cutoff until the slot departs.
inIssueWindow :: UTCTime -> UTCTime -> Bool
inIssueWindow now departure = issueTimeOn departure <= now && now < departure

-- | Send the OTP from an hour before departure: once a vehicle is assigned, or at the last call regardless.
otpDue :: UTCTime -> UTCTime -> Bool -> Bool
otpDue now departure hasVehicle =
  now < departure
    && now >= addUTCTime (negate otpLeadTime) departure
    && (hasVehicle || now >= addUTCTime (negate otpLastCall) departure)

-- | A day can be skipped or unskipped until its change cutoff. Without a known departure (timetable
-- unavailable) only days after today are open.
dayOpenForChange :: UTCTime -> DT.Day -> Maybe UTCTime -> DT.Day -> Bool
dayOpenForChange now today mbDeparture day
  | day < today = False
  | otherwise = case mbDeparture of
    Just departure -> now < issueTimeOn departure
    Nothing -> day > today

-- | Where a ticket for this departure has its startTime: between the issue time and the departure (with slack).
liveTicketWindow :: UTCTime -> (UTCTime, UTCTime)
liveTicketWindow departure = (addUTCTime (negate $ changeCutoff + 3600) departure, addUTCTime 3600 departure)

-- | The same, when the departure is unknown: any time that could belong to the service day.
dayTicketWindow :: Seconds -> DT.Day -> (UTCTime, UTCTime)
dayTicketWindow timeDiff day =
  (addUTCTime (negate changeCutoff) (localMidnightUtc timeDiff day), addUTCTime 3600 (localMidnightUtc timeDiff (DT.addDays 1 day)))

isLiveStatus :: DStatus.FRFSTicketBookingStatus -> Bool
isLiveStatus status = status `notElem` [DStatus.FAILED, DStatus.CANCELLED, DStatus.COUNTER_CANCELLED, DStatus.RESCHEDULED]

-- | Adds the days not already skipped, keeping existing entries (and their reasons) as they are.
addSkipDays :: [DPPP.SkipDay] -> [DT.Day] -> Maybe Text -> [DPPP.SkipDay]
addSkipDays existing days reason =
  let already = map (.day) existing
   in existing <> [DPPP.SkipDay {day, reason} | day <- nub days, day `notElem` already]

removeSkipDays :: [DPPP.SkipDay] -> [DT.Day] -> [DPPP.SkipDay]
removeSkipDays existing days = filter ((`notElem` days) . (.day)) existing

formatOtp :: Int -> Text
formatOtp n = T.pack (printf "%04d" (n `mod` 10000) :: String)

-- | The stored code is character(4); trim both sides so padding or stray spaces never reject a right code.
otpMatches :: Maybe Text -> Text -> Bool
otpMatches mbStored presented = case T.strip <$> mbStored of
  Just stored -> not (T.null stored) && stored == T.strip presented
  Nothing -> False

-- | Whether a trip serves pickup then drop, given the sequence numbers at which each stop occurs. GTFS lets a stop
-- occur more than once in a trip, so any drop occurrence after any pickup occurrence counts.
servesInOrder :: [Int] -> [Int] -> Bool
servesInOrder pickupSequences dropSequences = any (\pickup -> any (> pickup) dropSequences) pickupSequences

-- | The trip the term rides on a day: the day's reschedule when there is one, else the term's own slot.
slotOn :: DPPP.PurchasedPassPayment -> DT.Day -> Maybe Text
slotOn term = slotFor term.slotTripId term.slotChanges

slotFor :: Maybe Text -> [DPPP.SlotChange] -> DT.Day -> Maybe Text
slotFor ownSlot changes day = maybe ownSlot (Just . (.slotTripId)) (find ((== day) . (.day)) changes)

-- | Moves a day onto a slot, replacing any earlier reschedule of that day. Moving it back to the term's own slot
-- leaves no entry, so the list only ever holds real changes.
setSlotChange :: Maybe Text -> [DPPP.SlotChange] -> DT.Day -> Text -> [DPPP.SlotChange]
setSlotChange ownSlot existing day slot =
  filter ((/= day) . (.day)) existing <> [DPPP.SlotChange {day, slotTripId = slot} | Just slot /= ownSlot]

-- | Whether the term takes a seat on this slot that day: it covers the day, rides this slot then, and has not skipped it.
-- One seat per term (the issue job books quantity 1).
ridesSlotOn :: Text -> DT.Day -> DPPP.PurchasedPassPayment -> Bool
ridesSlotOn slot day term = ridesSlot slot day (term.startDate, term.endDate) term.slotTripId term.slotChanges term.skipDays

ridesSlot :: Text -> DT.Day -> (DT.Day, DT.Day) -> Maybe Text -> [DPPP.SlotChange] -> [DPPP.SkipDay] -> Bool
ridesSlot slot day (startDate, endDate) ownSlot changes skips =
  startDate <= day
    && day <= endDate
    && slotFor ownSlot changes day == Just slot
    && day `notElem` map (.day) skips

hasFreeSeat :: Int -> Int -> Bool
hasFreeSeat capacity booked = booked < capacity

-- | A day can move to another slot while both the slot it rides now and the new one are before their change cutoff.
-- Both departures must be known: when the timetable cannot place the current slot, its cutoff cannot be checked, so
-- the move is not open.
rescheduleOpen :: UTCTime -> DT.Day -> Maybe UTCTime -> UTCTime -> DT.Day -> Bool
rescheduleOpen now today mbCurrentDeparture newDeparture day = case mbCurrentDeparture of
  Nothing -> False
  Just currentDeparture -> dayOpenForChange now today (Just currentDeparture) day && now < issueTimeOn newDeparture
