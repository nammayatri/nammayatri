{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | A shared-route run as both platforms name it: the GTFS trip and its first departure, and the tickets that belong to it.
module SharedRunTests (tests) where

import "dynamic-offer-driver-app" API.Types.UI.SharedCab (SharedCabBookingOperation (..))
import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import "beckn-spec" Domain.Types.SharedRoute (RunRef (..), runTicketWindow, ticketOnRun)
import "dynamic-offer-driver-app" SharedLogic.SharedCab.RiderClient (BAPBookingOperationReq (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

-- | 07:30 UTC on 2026-11-25
firstDeparture :: UTCTime
firstDeparture = UTCTime (fromGregorian 2026 11 25) (7 * 3600 + 30 * 60)

run :: RunRef
run = RunRef {tripId = "T-0730", departure = firstDeparture}

hours :: Double -> UTCTime
hours h = addUTCTime (realToFrac (h * 3600)) firstDeparture

tests :: TestTree
tests =
  testGroup
    "shared run identity"
    [ testCase "the ticket window is 12 hours either side of the first departure" $
        runTicketWindow run @?= (hours (-12), hours 12),
      testGroup
        "a ticket is on the run"
        [ testCase "same trip, issued 5 hours before departure" $ ticketOnRun run (Just "T-0730") (Just (hours (-5))) @?= True,
          testCase "same trip, at departure" $ ticketOnRun run (Just "T-0730") (Just firstDeparture) @?= True,
          testCase "same trip, the rider boards an hour into the run" $ ticketOnRun run (Just "T-0730") (Just (hours 1)) @?= True,
          testCase "same trip, the window's start is inside" $ ticketOnRun run (Just "T-0730") (Just (hours (-12))) @?= True
        ],
      testGroup
        "a ticket is not on the run"
        [ testCase "same trip, the next day" $ ticketOnRun run (Just "T-0730") (Just (hours 24)) @?= False,
          testCase "same trip, the day before" $ ticketOnRun run (Just "T-0730") (Just (hours (-24))) @?= False,
          testCase "same trip, the window's end is outside" $ ticketOnRun run (Just "T-0730") (Just (hours 12)) @?= False,
          testCase "another trip, same time" $ ticketOnRun run (Just "T-0800") (Just firstDeparture) @?= False,
          testCase "no trip id" $ ticketOnRun run Nothing (Just firstDeparture) @?= False,
          testCase "no start time" $ ticketOnRun run (Just "T-0730") Nothing @?= False
        ],
      testCase "a run passenger's booking operation carries the run to rider-app" $
        (decode . encode) (BAPBookingOperationReq {operation = DROPPED, driverId = "d1", vehicleNumber = "KA01AB1234", reason = Nothing, otp = Nothing, runTripId = Just "T-0730", runDeparture = Just firstDeparture})
          @?= Just (object ["operation" .= ("DROPPED" :: String), "driverId" .= ("d1" :: String), "vehicleNumber" .= ("KA01AB1234" :: String), "reason" .= Null, "otp" .= Null, "runTripId" .= ("T-0730" :: String), "runDeparture" .= ("2026-11-25T07:30:00Z" :: String)])
    ]
