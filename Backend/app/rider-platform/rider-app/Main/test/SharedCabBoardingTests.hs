{-# LANGUAGE PackageImports #-}

module SharedCabBoardingTests (tests) where

import "rider-app" API.Types.UI.MultimodalConfirm (RiderLocationReq (..))
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import qualified "beckn-spec" Domain.Types.FRFSTicketStatus as TS
import Kernel.External.Maps.Types (LatLong (..))
import "rider-app" Lib.JourneyLeg.Common.FRFSJourneyUtils (VehicleProximity (..), riderNearVehicle)
import "rider-app" SharedLogic.SharedCab.Boarding (SharedCabBoardOutcome (..), UnknownCodeStep (..), bindingUnmoved, cabProximity, firstUse, forceHonoured, forcedNoLocation, unknownCodeStep)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "SharedCab boarding"
    [ testGroup
        "unknown code"
        [ testCase "no confirm: asks, never boards" $
            unknownCodeStep Nothing @?= AskToConfirmUnlisted,
          testCase "forceCheckIn False: still asks" $
            unknownCodeStep (Just False) @?= AskToConfirmUnlisted,
          testCase "explicit confirm: boards unlisted" $
            unknownCodeStep (Just True) @?= BoardUnlisted
        ],
      testGroup
        "forced check-in"
        [ testCase "the allocated cab" $
            forceHonoured (Just True) (Just "ML05A1234") "ML05A1234" @?= True,
          testCase "FINDING/FALLBACK booking, no cab yet" $
            forceHonoured (Just True) Nothing "ML05A1234" @?= True,
          testCase "re-bind to another cab: never forced from home" $
            forceHonoured (Just True) (Just "ML05A9999") "ML05A1234" @?= False,
          testCase "no force asked" $
            forceHonoured Nothing Nothing "ML05A1234" @?= False,
          testCase "only a forced boarding with no cab counts as a no-location boarding" $
            map forcedNoLocation [Nothing, Just "ML05A1234"] @?= [True, False]
        ],
      testGroup
        "H2: binding under the booking lock"
        [ testCase "a cab claimed since the pre-lock read aborts the boarding" $
            [ bindingUnmoved "ML05A1234" Nothing Nothing,
              bindingUnmoved "ML05A1234" Nothing (Just "ML05B2222"),
              bindingUnmoved "ML05A1234" (Just "ML05A1234") (Just "ML05A1234"),
              bindingUnmoved "ML05A1234" (Just "ML05A1234") (Just "ML05B2222"),
              bindingUnmoved "ML05A1234" (Just "ML05A1234") Nothing
            ]
              @?= [True, False, True, False, False],
          testCase "e2e B12: the tick claiming the booking for the SAME cab being boarded does not abort" $
            bindingUnmoved "ML05A1234" Nothing (Just "ML05A1234") @?= True
        ],
      testGroup
        "proximity (the bus boarding match)"
        [ testCase "a fresh cab ping is the anchor" $
            cabProximity now 10 (Just (ago 2, cab)) (Just stop) [at 1 cab] @?= Just (RiderDistance 0),
          testCase "a stale cab ping falls back to the boarding stop" $
            cabProximity now 10 (Just (ago 30, cab)) (Just stop) [at 1 stop] @?= Just (RiderDistance 0),
          testCase "the rider's point closest in time to the ping counts, not the newest" $
            cabProximity now 10 (Just (ago 6, cab)) (Just stop) [at 6 cab, at 1 stop] @?= Just (RiderDistance 0),
          testCase "the cab ping takes the 10 s bar; the rider's point keeps 300 s" $
            [ cabProximity now 10 (Just (ago 9, cab)) Nothing [at 9 cab],
              cabProximity now 10 (Just (ago 11, cab)) Nothing [at 11 cab],
              cabProximity now 10 (Just (ago 2, cab)) Nothing [at 200 cab],
              cabProximity now 10 (Just (ago 2, cab)) Nothing [at 301 cab]
            ]
              @?= [Just (RiderDistance 0), Nothing, Just (RiderDistance 0), Just NoRecentRiderPoint],
          testCase "a rider point 60 s old boards a cab; the bus, its bar on both windows, still refuses it" $
            [ cabProximity now 10 (Just (ago 2, cab)) Nothing [at 60 cab],
              Just (riderNearVehicle now 10 10 (ago 2, cab) [at 60 cab])
            ]
              @?= [Just (RiderDistance 0), Just NoRecentRiderPoint],
          testCase "no ping and no stop is no anchor" $
            cabProximity now 10 Nothing Nothing [at 1 cab] @?= Nothing
        ],
      testGroup
        "pt_booking_used"
        [ testCase "a boarding, or a degraded one, puts unused tickets in use" $
            map (firstUse [TS.ACTIVE]) [boarded, SharedCabDegraded] @?= [True, True],
          testCase "tickets already boarded (a re-bind or a repeat) or used are not a first use" $
            [firstUse [TS.INPROGRESS] boarded, firstUse [TS.USED] SharedCabDegraded] @?= [False, False],
          testCase "a proximity hold boards nothing" $
            firstUse [TS.ACTIVE] (SharedCabProximityHold Nothing "TOO_FAR_FROM_CAB") @?= False
        ]
    ]
  where
    boarded = SharedCabBoarded
    now = UTCTime (fromGregorian 2026 10 28) 36000
    ago sec = addUTCTime (negate sec) now
    at sec latLong = RiderLocationReq {currTime = ago sec, latLong}
    cab = LatLong 25.5788 91.8933
    stop = LatLong 25.5688 91.8833
