module OpenMeter.TransitionsSpec (openMeterTransitionsTests) where

import qualified API.Types.UI.OpenMeter as API
import qualified Data.Time as Time
import Domain.Types.OpenMeterRide (OpenMeterEndReason (..), OpenMeterRide (..), OpenMeterRideStatus (..))
import qualified "rider-app" Domain.Types.Person as DP
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Types.Common (Currency (..), Meters (..), Seconds (..))
import Kernel.Types.Error.BaseError.HTTPError (IsHTTPError (toErrorCode))
import Kernel.Types.Id (Id (..))
import SharedLogic.OpenMeter
import Test.Tasty
import Test.Tasty.HUnit

openMeterTransitionsTests :: TestTree
openMeterTransitionsTests =
  testGroup
    "Open meter transitions (SharedLogic.OpenMeter)"
    [ rideIdTests,
      startTests,
      heartbeatTests,
      endTests,
      cancelTests,
      autoEndTests,
      distanceTests
    ]

now :: Time.UTCTime
now = Time.UTCTime (Time.fromGregorian 2026 11 5) (Time.secondsToDiffTime 43200)

minutesAgo :: Integer -> Time.UTCTime
minutesAgo m = Time.addUTCTime (fromInteger (negate m * 60)) now

hoursAgo :: Integer -> Time.UTCTime
hoursAgo h = minutesAgo (h * 60)

p, q :: Id DP.Person
p = Id "rider-p"
q = Id "rider-q"

-- A RUNNING ride of rider p, started an hour ago, last heartbeat a minute ago.
ride :: Text -> OpenMeterRide
ride rid =
  OpenMeterRide
    { id = Id rid,
      personId = p,
      status = RUNNING,
      startTime = hoursAgo 1,
      serverStartedAt = hoursAgo 1,
      endTime = Nothing,
      lastHeartbeatAt = Just (minutesAgo 1),
      startLatLong = Just (LatLong 12.9 77.6),
      endLatLong = Nothing,
      lastLatLong = Just (LatLong 12.91 77.61),
      distanceMeters = Meters 1000,
      serverLinearDistanceMeters = Nothing,
      estimatedGapMeters = Meters 0,
      waitingTimeSeconds = Seconds 60,
      encodedPolyline = Nothing,
      pointsCount = 0,
      fare = Just 50,
      currency = Nothing,
      tariffSnapshot = Nothing,
      trackingConfig = Nothing,
      endReason = Nothing,
      merchantId = Nothing,
      merchantOperatingCityId = Nothing,
      createdAt = hoursAgo 1,
      updatedAt = minutesAgo 1
    }

r :: OpenMeterRide
r = ride "ride-r"

withStatus :: OpenMeterRideStatus -> OpenMeterRide -> OpenMeterRide
withStatus st x = x {status = st}

errorCodeOf :: IsHTTPError e => Either e a -> Either Text a
errorCodeOf = either (Left . toErrorCode) Right

heartbeatAt :: Time.UTCTime -> API.OpenMeterRideHeartbeatReq
heartbeatAt t =
  API.OpenMeterRideHeartbeatReq
    { sampledAt = t,
      distanceMeters = Meters 1500,
      waitingTimeSeconds = Seconds 90,
      lastLatLong = Just (LatLong 12.92 77.62),
      fare = Just 65
    }

endReq :: API.OpenMeterRideEndReq
endReq =
  API.OpenMeterRideEndReq
    { endTime = now,
      endLatLong = Just (LatLong 12.93 77.63),
      distanceMeters = Meters 2400,
      waitingTimeSeconds = Seconds 120,
      fare = Just 80,
      currency = Just INR,
      encodedPolyline = "???gE",
      pointsCount = 42,
      estimatedGapMeters = Meters 30,
      endReason = USER
    }

-- The ride id is the primary key of a varchar(36) column; anything that is not a UUID would be
-- accepted by KV and then lost when the drainer's Postgres insert fails.
rideIdTests :: TestTree
rideIdTests =
  testGroup
    "isValidRideId"
    [ testCase "start accepts a UUID ride id in either case" $ do
        isValidRideId (Id "3f0a8c2e-7b1d-4c5e-9a6f-0b2d4e6f8a1c") @?= True
        isValidRideId (Id "3F0A8C2E-7B1D-4C5E-9A6F-0B2D4E6F8A1C") @?= True,
      testCase "start rejects a ride id that is not a UUID" $ do
        isValidRideId (Id "") @?= False
        isValidRideId (Id "ride-r") @?= False
        isValidRideId (Id "3f0a8c2e7b1d4c5e9a6f0b2d4e6f8a1c") @?= False
        isValidRideId (Id "3f0a8c2e-7b1d-4c5e-9a6f-0b2d4e6f8a1g") @?= False
        isValidRideId (Id " 3f0a8c2e-7b1d-4c5e-9a6f-0b2d4e6f8a1") @?= False,
      testCase "start rejects a ride id longer than the 36-character id column" $
        isValidRideId (Id "3f0a8c2e-7b1d-4c5e-9a6f-0b2d4e6f8a1c0") @?= False
    ]

startTests :: TestTree
startTests =
  testGroup
    "decideStart"
    [ testCase "start is idempotent for same rideId" $
        decideStart p False now 12 (Just r {personId = p}) [] @?= ReturnExisting r,
      testCase "start for another person's id is not found" $
        decideStart p False now 12 (Just r {personId = q}) [] @?= RejectNotFound,
      testCase "start rejects second RUNNING ride when allowMultiple=false" $ do
        let running1 = ride "running-1"
        decideStart p False now 12 Nothing [running1] @?= RejectActive running1.id,
      testCase "start allows second ride when allowMultiple=true" $
        decideStart p True now 12 Nothing [ride "running-1"] @?= Create [],
      testCase "stale running ride is auto-ended on start" $ do
        let stale = (ride "stale") {lastHeartbeatAt = Just (hoursAgo 13)}
        decideStart p False now 12 Nothing [stale] @?= Create [stale.id]
    ]

heartbeatTests :: TestTree
heartbeatTests =
  testGroup
    "applyHeartbeat"
    [ testCase "heartbeat older than stored is ignored" $
        applyHeartbeat now (heartbeatAt (minutesAgo 2)) r @?= Right Nothing,
      testCase "heartbeat on ENDED is invalid state" $
        errorCodeOf (applyHeartbeat now (heartbeatAt now) (withStatus ENDED r)) @?= Left "OPEN_METER_RIDE_INVALID_STATE",
      testCase "newer heartbeat replaces the totals" $
        applyHeartbeat now (heartbeatAt now) r
          @?= Right
            ( Just
                r
                  { distanceMeters = Meters 1500,
                    waitingTimeSeconds = Seconds 90,
                    lastLatLong = Just (LatLong 12.92 77.62),
                    fare = Just 65,
                    lastHeartbeatAt = Just now,
                    updatedAt = now
                  }
            )
    ]

endTests :: TestTree
endTests =
  testGroup
    "applyEnd"
    [ testCase "end twice returns stored ride" $
        applyEnd now endReq (Meters 111) (withStatus ENDED r) @?= Right Nothing,
      testCase "end after AUTO_ENDED accepts client data" $
        applyEnd now endReq (Meters 111) (withStatus AUTO_ENDED r)
          @?= Right
            ( Just
                r
                  { status = ENDED,
                    endTime = Just now,
                    endLatLong = Just (LatLong 12.93 77.63),
                    distanceMeters = Meters 2400,
                    serverLinearDistanceMeters = Just (Meters 111),
                    waitingTimeSeconds = Seconds 120,
                    fare = Just 80,
                    currency = Just INR,
                    encodedPolyline = Just "???gE",
                    pointsCount = 42,
                    estimatedGapMeters = Meters 30,
                    endReason = Just USER,
                    updatedAt = now
                  }
            ),
      testCase "end on CANCELLED is invalid state" $
        errorCodeOf (applyEnd now endReq (Meters 111) (withStatus CANCELLED r)) @?= Left "OPEN_METER_RIDE_INVALID_STATE"
    ]

cancelTests :: TestTree
cancelTests =
  testGroup
    "applyCancel"
    [ testCase "cancel of a RUNNING ride marks it CANCELLED" $
        applyCancel now r @?= Right (Just r {status = CANCELLED, updatedAt = now}),
      testCase "cancel twice returns stored ride" $
        applyCancel now (withStatus CANCELLED r) @?= Right Nothing,
      testCase "cancel on ENDED is invalid state" $
        errorCodeOf (applyCancel now (withStatus ENDED r)) @?= Left "OPEN_METER_RIDE_INVALID_STATE"
    ]

-- Logout, a login that ends the rider's other sessions, and a stale ride found on start all close
-- a RUNNING ride the same way.
autoEndTests :: TestTree
autoEndTests =
  testGroup
    "applyAutoEnd"
    [ testCase "auto-end of a RUNNING ride ends it at its last heartbeat and keeps its totals" $
        applyAutoEnd now r @?= Just r {status = AUTO_ENDED, endTime = Just (minutesAgo 1), updatedAt = now},
      testCase "auto-end of a ride that never sent a heartbeat ends it at the server's start time" $ do
        -- The start reached the server 10 minutes after the phone's start time.
        let silent = r {lastHeartbeatAt = Nothing, serverStartedAt = minutesAgo 50}
        applyAutoEnd now silent @?= Just silent {status = AUTO_ENDED, endTime = Just (minutesAgo 50), updatedAt = now},
      testCase "auto-end leaves a ride that is not RUNNING as it is" $
        map (\st -> applyAutoEnd now (withStatus st r)) [ENDED, CANCELLED, AUTO_ENDED] @?= [Nothing, Nothing, Nothing],
      testCase "an auto-ended ride no longer blocks the rider's next start" $
        decideStart p False now 12 Nothing (maybeToList (applyAutoEnd now r)) @?= Create [],
      testCase "an auto-ended ride still takes the phone's END" $
        fmap (errorCodeOf . fmap (fmap (.status)) . applyEnd now endReq (Meters 111)) (applyAutoEnd now r)
          @?= Just (Right (Just ENDED))
    ]

distanceTests :: TestTree
distanceTests =
  testGroup
    "server distance"
    [ testCase "linear distance of a decoded polyline" $
        -- (0, 0) -> (0, 0.001): 0.001 degree of longitude on the equator is about 111.19 m.
        serverLinearDistance "???gE" @?= Just (Meters 111),
      testCase "an empty polyline has no distance" $
        serverLinearDistance "" @?= Just (Meters 0),
      testCase "a truncated polyline is rejected instead of crashing the decoder" $
        serverLinearDistance "???g" @?= Nothing,
      testCase "client distance more than 30% off the server's is a mismatch" $ do
        isDistanceMismatch (Meters 1400) (Meters 1000) @?= True
        isDistanceMismatch (Meters 1200) (Meters 1000) @?= False,
      testCase "no mismatch is reported when the server distance is zero" $
        isDistanceMismatch (Meters 500) (Meters 0) @?= False
    ]
