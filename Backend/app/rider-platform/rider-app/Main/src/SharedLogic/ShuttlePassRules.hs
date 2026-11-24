-- | Pure rules of the shuttle subscription, kept apart from IO so they can be unit-tested
-- (hunit-tests/src/ShuttlePassRulesTests.hs).
module SharedLogic.ShuttlePassRules
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
