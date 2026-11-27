-- | Rules from the Assam SOP "Picnic Journeys by Road: Picnic Season"
-- (No. TMV/E 872551/1, approval date still pending). Everything here is pure.
module Lib.Permit.Picnic.Rules
  ( DocumentFact (..),
    ScanInput (..),
    approvalLevelFor,
    requiredDocuments,
    documentsInvalidatedBy,
    picnicLimits,
    returnDeadline,
    departureInstant,
    returnInstant,
    checkForm,
    checkSubmission,
    checkApproval,
    evaluateScan,
    normaliseIdentifier,
    istDay,
    formatClock,
    pilotCity,
  )
where

import qualified Data.Char as Char
import qualified Data.Text as T
import Data.Time (Day, TimeOfDay (..), UTCTime (..), addDays, addUTCTime, diffUTCTime, timeOfDayToTime, timeToTimeOfDay)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Lib.Permit.Domain.Types.PermitApplication (ApprovalLevel (..), PermitStatus (..))
import Lib.Permit.Domain.Types.PermitApplicationDocument (PermitDocumentType (..))
import Lib.Permit.Picnic.Sunset (sunsetIST)
import Lib.Permit.Picnic.Types
import Lib.Permit.Types

data DocumentFact = DocumentFact
  { documentType :: PermitDocumentType,
    validUntil :: Maybe Day,
    lifetime :: Bool
  }

-- | The only operating city that may take picnic applications. The coordinates below,
-- and so the sunset deadline, are valid for it alone.
pilotCity :: Context.City
pilotCity = Context.City "Guwahati"

-- Pilot district: Kamrup Metropolitan. The coordinates drive the sunset time.
districtLatitude, districtLongitude :: Double
districtLatitude = 26.1445
districtLongitude = 91.7362

-- | The SOP caps "passengers" at the registered seating capacity. A registration
-- certificate's capacity normally includes the driver, so everyone on board
-- counts until the State says otherwise.
crewCountsTowardCapacity :: Bool
crewCountsTowardCapacity = True

earliestDeparture, latestReturn, intensifiedFrom, intensifiedTo :: TimeOfDay
earliestDeparture = TimeOfDay 7 0 0
latestReturn = TimeOfDay 18 0 0
intensifiedFrom = TimeOfDay 15 0 0
intensifiedTo = TimeOfDay 21 0 0

leadTimeSeconds :: Int
leadTimeSeconds = 7 * 86400

normalDriveMinutes, dcDriveMinutes, challanBlockCount, nearDeadlineMinutes, maxPassengers :: Int
normalDriveMinutes = 60
dcDriveMinutes = 90
challanBlockCount = 3
nearDeadlineMinutes = 30
maxPassengers = 80

istOffsetSeconds :: Int
istOffsetSeconds = 19800

istInstant :: Day -> TimeOfDay -> UTCTime
istInstant day time = addUTCTime (negate (fromIntegral istOffsetSeconds)) (UTCTime day (timeOfDayToTime time))

istParts :: UTCTime -> (Day, TimeOfDay)
istParts instant =
  let shifted = addUTCTime (fromIntegral istOffsetSeconds) instant
   in (utctDay shifted, timeToTimeOfDay (utctDayTime shifted))

istDay :: UTCTime -> Day
istDay = fst . istParts

parseClock :: Text -> Maybe TimeOfDay
parseClock text
  | T.length text == 5 = parseTimeM False defaultTimeLocale "%H:%M" (T.unpack text)
  | otherwise = Nothing

formatClock :: TimeOfDay -> Text
formatClock = T.pack . formatTime defaultTimeLocale "%H:%M"

minutesOfDay :: TimeOfDay -> Int
minutesOfDay (TimeOfDay hour minute _) = hour * 60 + minute

-- | SOP §2: home before sunset and in all cases by 6:00 PM, whichever is earlier.
returnDeadline :: Day -> TimeOfDay
returnDeadline day = min latestReturn (sunsetIST districtLatitude districtLongitude day)

departureInstant :: PicnicDetails -> Maybe UTCTime
departureInstant details = istInstant details.travelDate <$> parseClock details.departureTime

returnInstant :: PicnicDetails -> Maybe UTCTime
returnInstant details = istInstant details.travelDate <$> parseClock details.returnTime

-- | SOP §2 and §3.7: the District Commissioner alone decides 60 to 90 minutes.
approvalLevelFor :: PicnicDetails -> ApprovalLevel
approvalLevelFor details = if details.driveMinutes > normalDriveMinutes then DC else STANDARD

requiredDocuments :: PicnicVehicle -> [PermitDocumentType]
requiredDocuments vehicle =
  [RC, INSURANCE, PUC, ROAD_TAX, DRIVING_LICENCE, CHALLAN_STATUS]
    <> [FITNESS | vehicle.fitnessRequired]
    <> [VEHICLE_PERMIT | vehicle.permitRequired]
    <> [SPEED_LIMITER | vehicle.speedLimiterRequired]

-- | Photos that no longer describe the trip once the vehicle or the driver is replaced.
documentsInvalidatedBy :: Bool -> Bool -> [PermitDocumentType]
documentsInvalidatedBy vehicleChanged driverChanged =
  (if vehicleChanged then [RC, INSURANCE, PUC, ROAD_TAX, FITNESS, VEHICLE_PERMIT, SPEED_LIMITER, CHALLAN_STATUS] else [])
    <> [DRIVING_LICENCE | driverChanged]

-- | Vehicle registration and licence numbers compare without spaces, punctuation or case.
normaliseIdentifier :: Text -> Text
normaliseIdentifier = T.toUpper . T.filter Char.isAlphaNum

failure :: Text -> Text -> Text -> RuleFailure
failure = RuleFailure

-- | Fields that must be well formed before any rule, or even a draft, makes sense.
checkForm :: PicnicDetails -> [RuleFailure]
checkForm details =
  concat
    [ required "Group leader name" 80 details.leaderName,
      required "Starting point" 200 details.startPoint,
      required "Destination district" 80 details.destinationDistrict,
      required "Picnic spot" 200 details.spot,
      required "Route to the spot" 600 details.routeOut,
      required "Return route" 600 details.routeBack,
      required "Vehicle registration number" 20 (normaliseIdentifier details.vehicle.registrationNo),
      required "Vehicle owner name" 80 details.vehicle.ownerName,
      required "Driver name" 80 details.driver.name,
      required "Driving licence number" 24 (normaliseIdentifier details.driver.licenceNo),
      required "Licence class" 40 details.driver.licenceClass,
      mobile "Vehicle owner mobile" details.vehicle.ownerMobileNumber,
      mobile "Driver mobile" details.driver.mobileNumber,
      clock "Departure time" details.departureTime,
      clock "Return time" details.returnTime,
      [invalid "Driving time must be between 1 and 600 minutes." | details.driveMinutes < 1 || details.driveMinutes > 600],
      [invalid "Seating capacity must be between 1 and 100." | details.vehicle.seatingCapacity < 1 || details.vehicle.seatingCapacity > 100],
      [invalid "Challan numbers cannot be negative." | any (< 0) [details.challans.onRecord, details.challans.unpaid, details.challans.unpaidAmount]],
      [invalid "Unpaid challans cannot exceed challans on record." | details.challans.unpaid > details.challans.onRecord],
      [invalid "List every passenger, including the group leader." | null details.passengerNames],
      [invalid ("At most " <> show maxPassengers <> " passengers.") | length details.passengerNames > maxPassengers],
      concatMap (required "Passenger name" 80) (take maxPassengers details.passengerNames),
      maybe [] (required "Handyman name" 80) details.handymanName
    ]
  where
    invalid = failure "FORM_INVALID" "FORM"
    required label limit value
      | T.null (T.strip value) = [invalid (label <> " is required.")]
      | T.length value > limit = [invalid (label <> " is too long.")]
      | otherwise = []
    mobile label value =
      [invalid (label <> " must be a 10-digit mobile number.") | T.length value /= 10 || not (T.all Char.isDigit value)]
    clock label value = [invalid (label <> " must be a time in HH:MM.") | isNothing (parseClock value)]

checkTiming :: PicnicDetails -> [RuleFailure]
checkTiming details =
  case (parseClock details.departureTime, parseClock details.returnTime) of
    (Just departure, Just comeBack) ->
      let deadline = returnDeadline details.travelDate
       in concat
            [ [failure "EARLY_DEPARTURE" "§2" ("Departure cannot be before " <> formatClock earliestDeparture <> ".") | departure < earliestDeparture],
              [ failure "LATE_RETURN" "§2" ("The group must be home by " <> formatClock deadline <> " on " <> show details.travelDate <> " (sunset or 18:00, whichever is earlier).")
                | comeBack > deadline
              ],
              [ failure "RETURN_TOO_SOON" "§2" ("The return time leaves no room for " <> show details.driveMinutes <> " minutes of driving each way.")
                | minutesOfDay comeBack < minutesOfDay departure + 2 * details.driveMinutes
              ]
            ]
    _ -> [failure "FORM_INVALID" "FORM" "Departure and return times must be in HH:MM."]

checkLeadTime :: UTCTime -> PicnicDetails -> [RuleFailure]
checkLeadTime now details =
  [ failure "LEAD_TIME" "§3.1" "Apply at least 7 days before departure."
    | maybe True (\departure -> diffUTCTime departure now < fromIntegral leadTimeSeconds) (departureInstant details)
  ]

checkDistance :: PicnicDetails -> [RuleFailure]
checkDistance details =
  [ failure "TOO_FAR" "§2" ("Journeys over " <> show dcDriveMinutes <> " minutes' driving time are not permitted.")
    | details.driveMinutes > dcDriveMinutes
  ]

-- | SOP §1.
excludedCategories :: [PicnicVehicleCategory]
excludedCategories = [GOODS, TRUCK, TRACTOR, TRAILER]

checkVehicle :: PicnicDetails -> [RuleFailure]
checkVehicle details =
  concat
    [ [ failure "EXCLUDED_VEHICLE" "§1" "Goods vehicles, trucks, tractors and trailers cannot carry picnic passengers."
        | details.vehicle.category `elem` excludedCategories
      ],
      [ failure "OVER_CAPACITY" "§2" (show load <> " people counted against a registered seating capacity of " <> show details.vehicle.seatingCapacity <> ".")
        | load > details.vehicle.seatingCapacity
      ]
    ]
  where
    crew = if crewCountsTowardCapacity then 1 + (if isJust details.handymanName then 1 else 0) else 0
    load = length details.passengerNames + crew

-- | SOP §3.2: both conditions apply.
checkChallans :: PicnicDetails -> [RuleFailure]
checkChallans details =
  concat
    [ [failure "UNPAID_CHALLAN" "§3.2" "All unpaid challans must be cleared first." | details.challans.unpaid > 0],
      [ failure "CHALLAN_COUNT" "§3.2" (show challanBlockCount <> " or more challans on record blocks approval, even if paid.")
        | details.challans.onRecord >= challanBlockCount
      ]
    ]

checkDriver :: PicnicDriver -> [RuleFailure]
checkDriver driver =
  [ failure failureCode "§3.3" text
    | (broken, failureCode, text) <-
        [ (not driver.licenceClassCovers, "LICENCE_CLASS", "The licence class does not cover this vehicle."),
          (driver.suspended, "LICENCE_SUSPENDED", "The driving licence has an active suspension."),
          (driver.recentConviction, "DRIVER_CONVICTION", "Drink-driving or dangerous-driving conviction in the last three years."),
          (not driver.medicallyFit, "DRIVER_UNFIT", "The driver is not declared physically and mentally fit."),
          (not driver.knowsRoute, "ROUTE_UNFAMILIAR", "The driver is not familiar with the route.")
        ],
      broken
  ]

checkDocuments :: PicnicDetails -> [DocumentFact] -> [RuleFailure]
checkDocuments details facts = concatMap check (requiredDocuments details.vehicle)
  where
    check required =
      case find (\fact -> fact.documentType == required) facts of
        Nothing -> [failure "DOCUMENT_MISSING" "§3.2" (show required <> ": photo is missing.")]
        Just fact
          | required == CHALLAN_STATUS -> []
          | required == ROAD_TAX && fact.lifetime -> []
          | maybe False (>= details.travelDate) fact.validUntil -> []
          | otherwise -> [failure "DOCUMENT_EXPIRED" "§3.2" (show required <> ": not valid on the travel date.")]

checkTrip :: PicnicDetails -> [DocumentFact] -> [RuleFailure]
checkTrip details facts =
  concat
    [ checkTiming details,
      checkDistance details,
      checkVehicle details,
      checkChallans details,
      checkDriver details.driver,
      checkDocuments details facts
    ]

checkSubmission :: UTCTime -> PicnicDetails -> [DocumentFact] -> [RuleFailure]
checkSubmission now details facts = checkForm details <> checkLeadTime now details <> checkTrip details facts

-- | Checked again before approval. Lead time was settled at submission.
checkApproval :: UTCTime -> PicnicDetails -> [DocumentFact] -> [RuleFailure]
checkApproval now details facts =
  checkTrip details facts
    <> [ failure "DEPARTURE_PASSED" "§2" "The departure time has passed; this trip cannot be approved."
         | maybe True (<= now) (departureInstant details)
       ]

-- | The first date on which a trip can still leave after the notice ends and be home
-- by that day's deadline. Late in the day this is the day after the notice ends.
firstTravelDate :: UTCTime -> Day
firstTravelDate noticeEnds
  | firstDeparture + shortestTripMinutes <= minutesOfDay (returnDeadline day) = day
  | otherwise = addDays 1 day
  where
    (day, noticeTime) = istParts noticeEnds
    -- Departure is typed to the minute, so it is the first whole minute after the notice.
    firstDeparture = max (minutesOfDay earliestDeparture) (minutesOfDay noticeTime + 1)
    -- One minute of driving each way, the least the form accepts.
    shortestTripMinutes = 2

picnicLimits :: UTCTime -> Maybe Day -> PicnicLimits
picnicLimits now mbTravelDate =
  let noticeEnds = addUTCTime (fromIntegral leadTimeSeconds) now
   in PicnicLimits
        { earliestTravelDate = firstTravelDate noticeEnds,
          earliestDepartureAt = noticeEnds,
          earliestDepartureTime = formatClock earliestDeparture,
          latestReturnTime = formatClock . returnDeadline <$> mbTravelDate,
          standardDriveMinutes = normalDriveMinutes,
          maxDriveMinutes = dcDriveMinutes,
          blockingChallanCount = challanBlockCount,
          excludedVehicleCategories = excludedCategories,
          crewTakesSeats = crewCountsTowardCapacity
        }

data ScanInput = ScanInput
  { now :: UTCTime,
    status :: PermitStatus,
    details :: PicnicDetails,
    scannedFromQr :: Bool
  }

-- | Returns the colour, the reasons and whether checks are intensified (SOP §3.6).
evaluateScan :: ScanInput -> (ScanResult, [ScanReason], Bool)
evaluateScan input = (colour, redReasons <> amberReasons, intensified)
  where
    (today, clockNow) = istParts input.now
    trip = input.details
    deadline = returnDeadline trip.travelDate
    isToday = today == trip.travelDate
    intensified = clockNow >= intensifiedFrom && clockNow <= intensifiedTo
    redReasons =
      concat
        [ [ScanReason "NOT_APPROVED" ("Application is " <> T.toLower (show input.status) <> ". Travel is not permitted.") | input.status /= APPROVED],
          [ScanReason "WRONG_DATE" ("Pass is for " <> show trip.travelDate <> ", not today.") | not isToday],
          [ScanReason "EARLY_DEPARTURE" ("Travel before " <> formatClock earliestDeparture <> " is not permitted.") | isToday && clockNow < earliestDeparture],
          [ScanReason "LATE_RETURN" ("The group had to be home by " <> formatClock deadline <> ".") | isToday && clockNow > deadline]
        ]
    amberReasons =
      concat
        [ [ScanReason "MANUAL_LOOKUP" "Looked up by reference number; the QR code was not scanned." | not input.scannedFromQr],
          if isToday && clockNow >= earliestDeparture && clockNow <= deadline then timedReasons else []
        ]
    timedReasons =
      concat
        [ [ScanReason "BEFORE_DEPARTURE" ("Approved departure is " <> trip.departureTime <> ".") | maybe False (clockNow <) (parseClock trip.departureTime)],
          [ScanReason "AFTER_PLANNED_RETURN" ("Planned return was " <> trip.returnTime <> ".") | maybe False (clockNow >) (parseClock trip.returnTime)],
          [ ScanReason "NEAR_DEADLINE" ("The group must be home by " <> formatClock deadline <> ".")
            | minutesOfDay deadline - minutesOfDay clockNow <= nearDeadlineMinutes
          ]
        ]
    colour
      | not (null redReasons) = RED
      | not (null amberReasons) = AMBER
      | otherwise = GREEN
