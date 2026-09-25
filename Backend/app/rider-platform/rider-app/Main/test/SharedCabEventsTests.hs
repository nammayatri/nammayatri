{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabEventsTests (tests) where

import Data.Aeson (object, toJSON, (.=))
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import "rider-app" SharedLogic.SharedCab.Events
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 9 25) 0

tests :: TestTree
tests =
  testGroup
    "SharedCab Kafka events (05 §7)"
    [ testCase "session event: flat envelope plus its own fields" $
        toJSON (sessionEvent (RouteChanged "SC-MAWLAI-F") "ML05A1234" "SC-MAWLAI-R" "d1" t0)
          @?= object
            [ "event" .= ("route_changed" :: Text),
              "at" .= t0,
              "vehicleNumber" .= ("ML05A1234" :: Text),
              "bookingId" .= (Nothing :: Maybe Text),
              "routeCode" .= ("SC-MAWLAI-R" :: Text),
              "driverId" .= ("d1" :: Text),
              "fromRoute" .= ("SC-MAWLAI-F" :: Text)
            ],
      testCase "booking event: enum payloads use the 05 §7 spellings" $
        toJSON (bookingEvent (AllocationClosed "TIMEOUT" BlameDriver) "b1" (Just "ML05A1234") Nothing t0)
          @?= object
            [ "event" .= ("allocation_closed" :: Text),
              "at" .= t0,
              "vehicleNumber" .= ("ML05A1234" :: Text),
              "bookingId" .= ("b1" :: Text),
              "routeCode" .= (Nothing :: Maybe Text),
              "driverId" .= (Nothing :: Maybe Text),
              "outcome" .= ("TIMEOUT" :: Text),
              "blame" .= ("driver" :: Text)
            ],
      testCase "every kind has its 05 §7 name" $
        map eventName [SessionStarted, Resumed, Boarded ByFallbackR10, Dropped DroppedByTick, NoShow, SeatLost, InvariantViolation "r" "d"]
          @?= ["session_started", "resumed", "boarded", "dropped", "no_show", "seat_lost", "invariant_violation"]
    ]
