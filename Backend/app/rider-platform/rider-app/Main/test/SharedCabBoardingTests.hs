{-# LANGUAGE PackageImports #-}

module SharedCabBoardingTests (tests) where

import "rider-app" API.Types.UI.MultimodalConfirm (RiderLocationReq (..))
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Kernel.External.Maps.Types (LatLong (..))
import "rider-app" Lib.JourneyLeg.Common.FRFSJourneyUtils (VehicleProximity (..))
import "rider-app" SharedLogic.SharedCab.Boarding (UnknownCodeStep (..), bindingUnmoved, cabProximity, forceHonoured, forcedNoLocation, unknownCodeStep)
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
          testCase "a rider point older than the bar is no location" $
            cabProximity now 10 (Just (ago 2, cab)) (Just stop) [at 20 cab] @?= Just NoRecentRiderPoint,
          testCase "no ping and no stop is no anchor" $
            cabProximity now 10 Nothing Nothing [at 1 cab] @?= Nothing
        ]
    ]
  where
    now = UTCTime (fromGregorian 2026 10 28) 36000
    ago sec = addUTCTime (negate sec) now
    at sec latLong = RiderLocationReq {currTime = ago sec, latLong}
    cab = LatLong 25.5788 91.8933
    stop = LatLong 25.5688 91.8833
