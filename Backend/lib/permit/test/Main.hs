-- | Tests of the picnic SOP rules. They are pure and need no services.
module Main (main) where

import Data.Time (Day, TimeOfDay (..), UTCTime (..), addUTCTime, fromGregorian, timeOfDayToTime)
import Kernel.Prelude
import Lib.Permit.Domain.Types.PermitApplication (ApprovalLevel (..), PermitStatus (..))
import Lib.Permit.Domain.Types.PermitApplicationDocument (PermitDocumentType (..))
import Lib.Permit.Picnic.Rules
import Lib.Permit.Picnic.Sunset (sunsetIST)
import Lib.Permit.Picnic.Types
import Lib.Permit.Types
import Test.Hspec

-- A winter date, when sunset comes before 6:00 PM, and a summer date, when it does not.
winterDay, summerDay :: Day
winterDay = fromGregorian 2026 12 4
summerDay = fromGregorian 2027 6 21

ist :: Day -> Int -> Int -> UTCTime
ist day hour minute = addUTCTime (negate 19800) (UTCTime day (timeOfDayToTime (TimeOfDay hour minute 0)))

minutes :: TimeOfDay -> Int
minutes (TimeOfDay hour minute _) = hour * 60 + minute

clockPlus :: Int -> TimeOfDay -> Text
clockPlus delta time =
  let total = minutes time + delta
   in formatClock (TimeOfDay (total `div` 60) (total `mod` 60) 0)

trip :: PicnicDetails
trip =
  PicnicDetails
    { leaderName = "Leader",
      travelDate = winterDay,
      departureTime = "08:00",
      returnTime = "15:00",
      startPoint = "Start",
      destinationDistrict = "Kamrup",
      spot = "Spot",
      routeOut = "Out",
      routeBack = "Back",
      driveMinutes = 40,
      vehicle =
        PicnicVehicle
          { registrationNo = "AS01AB1234",
            category = SUV_VAN,
            usage = PRIVATE_USE,
            seatingCapacity = 7,
            ownerName = "Owner",
            ownerMobileNumber = "9000000001",
            fitnessRequired = False,
            permitRequired = False,
            speedLimiterRequired = False
          },
      challans = PicnicChallans {onRecord = 0, unpaid = 0, unpaidAmount = 0},
      driver = goodDriver,
      handymanName = Nothing,
      passengerNames = ["Leader", "Second", "Third"]
    }

goodDriver :: PicnicDriver
goodDriver =
  PicnicDriver
    { name = "Driver",
      mobileNumber = "9000000002",
      licenceNo = "AS0120200001234",
      licenceClass = "LMV",
      licenceClassCovers = True,
      suspended = False,
      recentConviction = False,
      medicallyFit = True,
      knowsRoute = True
    }

fact :: PermitDocumentType -> Maybe Day -> DocumentFact
fact kind expiry = DocumentFact {documentType = kind, validUntil = expiry, lifetime = False}

allDocuments :: PicnicDetails -> [DocumentFact]
allDocuments details = map (\kind -> fact kind (Just details.travelDate)) (requiredDocuments details.vehicle)

-- Twenty days before the winter trip.
longBefore :: UTCTime
longBefore = ist (fromGregorian 2026 11 14) 9 0

codes :: [RuleFailure] -> [Text]
codes = map (.code)

submissionCodes :: PicnicDetails -> [Text]
submissionCodes details = codes (checkSubmission longBefore details (allDocuments details))

withVehicle :: (PicnicVehicle -> PicnicVehicle) -> PicnicDetails
withVehicle change = trip {vehicle = change trip.vehicle}

withDriver :: PicnicDriver -> PicnicDetails
withDriver changed = trip {driver = changed}

people :: Int -> [Text]
people count = map (\n -> "Person " <> show n) [1 .. count]

scanAt :: UTCTime -> PermitStatus -> Bool -> (ScanResult, [Text], Bool)
scanAt instant permitStatus fromQr =
  let (colour, reasons, intensified) =
        evaluateScan ScanInput {now = instant, status = permitStatus, details = trip, scannedFromQr = fromQr}
   in (colour, map (.code) reasons, intensified)

main :: IO ()
main = hspec $ do
  describe "sunset in the pilot district" $ do
    -- Reference: api.sunrise-sunset.org for 26.1445 N, 91.7362 E, converted to IST.
    let reference =
          [ (fromGregorian 2026 12 1, TimeOfDay 16 30 51),
            (fromGregorian 2026 12 4, TimeOfDay 16 31 1),
            (fromGregorian 2026 12 15, TimeOfDay 16 33 33),
            (fromGregorian 2026 12 21, TimeOfDay 16 36 5),
            (fromGregorian 2027 1 1, TimeOfDay 16 42 28),
            (fromGregorian 2027 1 15, TimeOfDay 16 52 37),
            (fromGregorian 2027 2 14, TimeOfDay 17 14 55),
            (fromGregorian 2027 6 21, TimeOfDay 18 18 7)
          ]
    forM_ reference $ \(day, published) ->
      -- The SOP says "before sunset", so the calculated minute must never be later.
      it ("is not after the reference, and at most two minutes before it, on " <> show day) $
        (minutes (sunsetIST 26.1445 91.7362 day) - minutes published) `shouldSatisfy` (\late -> late <= 0 && late >= -2)
    it "is the return deadline in winter" $
      returnDeadline winterDay `shouldBe` sunsetIST 26.1445 91.7362 winterDay
    it "gives way to 18:00 when it comes later" $
      returnDeadline summerDay `shouldBe` TimeOfDay 18 0 0

  describe "a complete, lawful trip" $
    it "passes every submission rule" $
      submissionCodes trip `shouldBe` []

  describe "timing (SOP 2)" $ do
    it "refuses departure at 06:59" $
      submissionCodes trip {departureTime = "06:59"} `shouldContain` ["EARLY_DEPARTURE"]
    it "allows departure at 07:00" $
      submissionCodes trip {departureTime = "07:00"} `shouldBe` []
    it "allows a return exactly at the winter deadline" $
      submissionCodes trip {returnTime = clockPlus 0 (returnDeadline winterDay)} `shouldBe` []
    it "refuses a return one minute after the winter deadline" $
      submissionCodes trip {returnTime = clockPlus 1 (returnDeadline winterDay)} `shouldBe` ["LATE_RETURN"]
    it "allows 18:00 and refuses 18:01 in summer" $ do
      let summer = trip {travelDate = summerDay, returnTime = "18:00"}
      submissionCodes summer `shouldBe` []
      submissionCodes summer {returnTime = "18:01"} `shouldBe` ["LATE_RETURN"]
    it "refuses a return that leaves no time to drive both ways" $
      submissionCodes trip {returnTime = "09:19"} `shouldBe` ["RETURN_TOO_SOON"]

  describe "seven days' notice (SOP 3.1)" $ do
    it "allows exactly 168 hours" $
      codes (checkSubmission (ist (fromGregorian 2026 11 27) 8 0) trip (allDocuments trip)) `shouldBe` []
    it "refuses one minute less" $
      codes (checkSubmission (ist (fromGregorian 2026 11 27) 8 1) trip (allDocuments trip)) `shouldBe` ["LEAD_TIME"]

  describe "driving time (SOP 2 and 3.7)" $ do
    it "needs no DC at 60 minutes" $
      approvalLevelFor trip {driveMinutes = 60} `shouldBe` STANDARD
    it "needs the DC at 61 and 90 minutes" $
      map (\m -> approvalLevelFor trip {driveMinutes = m}) [61, 90] `shouldBe` [DC, DC]
    it "allows 90 minutes" $
      submissionCodes trip {driveMinutes = 90} `shouldBe` []
    it "refuses 91 minutes" $
      submissionCodes trip {driveMinutes = 91} `shouldBe` ["TOO_FAR"]

  describe "seating (SOP 2)" $ do
    it "allows passengers plus driver to fill the seats" $
      submissionCodes trip {passengerNames = people 6} `shouldBe` []
    it "refuses one person over" $
      submissionCodes trip {passengerNames = people 7} `shouldBe` ["OVER_CAPACITY"]
    it "counts the handyman" $
      submissionCodes trip {passengerNames = people 6, handymanName = Just "Helper"} `shouldBe` ["OVER_CAPACITY"]

  describe "excluded vehicles (SOP 1)" $
    forM_ [GOODS, TRUCK, TRACTOR, TRAILER] $ \kind ->
      it ("refuses " <> show kind) $
        submissionCodes (withVehicle (\v -> v {category = kind})) `shouldBe` ["EXCLUDED_VEHICLE"]

  describe "challans (SOP 3.2, both conditions apply)" $ do
    it "refuses a single unpaid challan" $
      submissionCodes trip {challans = PicnicChallans 1 1 500} `shouldBe` ["UNPAID_CHALLAN"]
    it "allows two paid challans on record" $
      submissionCodes trip {challans = PicnicChallans 2 0 0} `shouldBe` []
    it "refuses three on record even when all are paid" $
      submissionCodes trip {challans = PicnicChallans 3 0 0} `shouldBe` ["CHALLAN_COUNT"]

  describe "driver (SOP 3.3)" $ do
    let cases =
          [ ("LICENCE_CLASS", goodDriver {licenceClassCovers = False}),
            ("LICENCE_SUSPENDED", goodDriver {suspended = True}),
            ("DRIVER_CONVICTION", goodDriver {recentConviction = True}),
            ("DRIVER_UNFIT", goodDriver {medicallyFit = False}),
            ("ROUTE_UNFAMILIAR", goodDriver {knowsRoute = False})
          ]
    forM_ cases $ \(expected, changed) ->
      it ("reports " <> toString' expected) $
        submissionCodes (withDriver changed) `shouldBe` [expected]

  describe "documents (SOP 3.2)" $ do
    let everything = withVehicle (\v -> v {fitnessRequired = True, permitRequired = True, speedLimiterRequired = True})
        others kind = filter (\f -> f.documentType /= kind) (allDocuments everything)
        check facts = codes (checkSubmission longBefore everything facts)
    it "asks for every certificate that applies" $
      length (requiredDocuments everything.vehicle) `shouldBe` 9
    forM_ (requiredDocuments everything.vehicle) $ \kind ->
      it ("needs a photo of " <> show kind) $
        check (others kind) `shouldBe` ["DOCUMENT_MISSING"]
    forM_ (filter (/= CHALLAN_STATUS) (requiredDocuments everything.vehicle)) $ \kind ->
      it ("refuses " <> show kind <> " expiring the day before travel") $
        check (fact kind (Just (fromGregorian 2026 12 3)) : others kind) `shouldBe` ["DOCUMENT_EXPIRED"]
    it "accepts lifetime road tax" $
      check (DocumentFact ROAD_TAX Nothing True : others ROAD_TAX) `shouldBe` []
    it "does not accept lifetime in place of another expiry date" $
      check (DocumentFact INSURANCE Nothing True : others INSURANCE) `shouldBe` ["DOCUMENT_EXPIRED"]

  describe "replacing the vehicle or driver in a draft" $ do
    it "keeps every photo when neither changes" $
      documentsInvalidatedBy False False `shouldBe` []
    it "drops only the licence for a new driver" $
      documentsInvalidatedBy False True `shouldBe` [DRIVING_LICENCE]
    it "drops every vehicle photo, and keeps the licence, for a new vehicle" $ do
      let dropped = documentsInvalidatedBy True False
      dropped `shouldNotContain` [DRIVING_LICENCE]
      length dropped `shouldBe` 8
    it "leaves no photo behind when both change" $
      length (documentsInvalidatedBy True True) `shouldBe` 9

  describe "limits sent to the form" $ do
    let limits = picnicLimits (ist (fromGregorian 2026 11 27) 20 30) (Just winterDay)
        firstDateFrom hour minute = (picnicLimits (ist (fromGregorian 2026 11 27) hour minute) Nothing).earliestTravelDate
    it "gives the day the notice ends when a trip can still be made that day" $ do
      firstDateFrom 0 5 `shouldBe` fromGregorian 2026 12 4
      firstDateFrom 9 0 `shouldBe` fromGregorian 2026 12 4
    it "moves to the next day once no trip fits before the deadline" $ do
      firstDateFrom 20 30 `shouldBe` fromGregorian 2026 12 5
      limits.earliestTravelDate `shouldBe` fromGregorian 2026 12 5
    it "changes day exactly when the last possible departure is gone" $ do
      let deadline = returnDeadline (fromGregorian 2026 12 4)
          minutesBefore back = let total = minutes deadline - back in firstDateFrom (total `div` 60) (total `mod` 60)
      minutesBefore 3 `shouldBe` fromGregorian 2026 12 4
      minutesBefore 2 `shouldBe` fromGregorian 2026 12 5
    it "gives the exact instant the notice ends" $
      limits.earliestDepartureAt `shouldBe` ist (fromGregorian 2026 12 4) 20 30
    it "gives the same return deadline the rules apply" $
      limits.latestReturnTime `shouldBe` Just (formatClock (returnDeadline winterDay))
    it "gives no deadline until a date is chosen" $
      (picnicLimits longBefore Nothing).latestReturnTime `shouldBe` Nothing

  describe "form checks" $ do
    it "refuses a time that is not HH:MM" $
      codes (checkForm trip {departureTime = "7:30"}) `shouldBe` ["FORM_INVALID"]
    it "refuses a mobile number that is not 10 digits" $
      codes (checkForm (withVehicle (\v -> v {ownerMobileNumber = "12345"}))) `shouldBe` ["FORM_INVALID"]
    it "needs at least one passenger" $
      codes (checkForm trip {passengerNames = []}) `shouldBe` ["FORM_INVALID"]
    it "compares registration numbers without spaces, punctuation or case" $
      normaliseIdentifier " as-01 ab 1234 " `shouldBe` "AS01AB1234"

  describe "approval" $ do
    it "does not apply the seven days' notice again" $
      codes (checkApproval (ist (fromGregorian 2026 12 3) 12 0) trip (allDocuments trip)) `shouldBe` []
    it "is refused once departure has passed" $
      codes (checkApproval (ist winterDay 8 0) trip (allDocuments trip)) `shouldBe` ["DEPARTURE_PASSED"]

  describe "scan result" $ do
    let deadline = returnDeadline winterDay
        today hour minute = ist winterDay hour minute
        minutesEarly delta = let total = minutes deadline - delta in today (total `div` 60) (total `mod` 60)
    it "is green on the day, between departure and planned return" $
      scanAt (today 10 0) APPROVED True `shouldBe` (GREEN, [], False)
    it "is green at the departure and planned return times" $ do
      scanAt (today 8 0) APPROVED True `shouldBe` (GREEN, [], False)
      scanAt (today 15 0) APPROVED True `shouldBe` (GREEN, [], True)
    it "is amber before the planned departure" $
      scanAt (today 7 0) APPROVED True `shouldBe` (AMBER, ["BEFORE_DEPARTURE"], False)
    it "is red before 07:00" $
      scanAt (today 6 59) APPROVED True `shouldBe` (RED, ["EARLY_DEPARTURE"], False)
    it "is amber after the planned return" $
      scanAt (today 15 1) APPROVED True `shouldBe` (AMBER, ["AFTER_PLANNED_RETURN"], True)
    it "adds a warning in the last 30 minutes before the deadline" $
      scanAt (minutesEarly 30) APPROVED True `shouldBe` (AMBER, ["AFTER_PLANNED_RETURN", "NEAR_DEADLINE"], True)
    it "is still amber at the deadline" $
      scanAt (minutesEarly 0) APPROVED True `shouldBe` (AMBER, ["AFTER_PLANNED_RETURN", "NEAR_DEADLINE"], True)
    it "is red one minute after the deadline" $
      scanAt (minutesEarly (-1)) APPROVED True `shouldBe` (RED, ["LATE_RETURN"], True)
    it "intensifies checks from 15:00 to 21:00" $
      map (\(h, m) -> let (_, _, flag) = scanAt (today h m) APPROVED True in flag) [(14, 59), (15, 0), (21, 0), (21, 1)]
        `shouldBe` [False, True, True, False]
    it "is red on another date" $
      scanAt (ist (fromGregorian 2026 12 3) 10 0) APPROVED True `shouldBe` (RED, ["WRONG_DATE"], False)
    forM_ [SUBMITTED, REJECTED, WITHDRAWN] $ \permitStatus ->
      it ("is red when the application is " <> show permitStatus) $
        scanAt (today 10 0) permitStatus True `shouldBe` (RED, ["NOT_APPROVED"], False)
    it "is amber when found by reference number instead of the QR code" $
      scanAt (today 10 0) APPROVED False `shouldBe` (AMBER, ["MANUAL_LOOKUP"], False)
  where
    toString' :: Text -> String
    toString' = show
