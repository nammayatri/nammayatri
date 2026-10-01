{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabPlateTests (tests) where

import "rider-app" SharedLogic.SharedCab.Plate (canonicalisePlate)
import "rider-app" SharedLogic.SharedCab.SpotBooking (CodeResolution (..), WalkUpChoice (..), cabRouteRequest, chooseWalkUp, isStickerCode, probeBus, resolveCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "SharedCab plate canonicaliser"
    [ testCase "lowercase with spaces and hyphens" $
        canonicalisePlate "ml 05-a 1234" @?= "ML05A1234",
      testCase "dots and tabs are stripped" $
        canonicalisePlate "ml.05\ta.1234" @?= "ML05A1234",
      testCase "already canonical is unchanged" $
        canonicalisePlate "ML05A1234" @?= "ML05A1234",
      testCase "empty stays empty" $
        canonicalisePlate "" @?= "",
      testGroup
        "walk-up: a typed sticker code resolves among the live cabs"
        [ testCase "exactly one cab ends in the code" $ resolveCode "9999" ["ML05A9999", "ML05B8888"] @?= OneCab "ML05A9999",
          testCase "no cab ends in the code" $ resolveCode "1234" ["ML05A9999", "ML05B8888"] @?= NoCab,
          testCase "two cabs end in the code: the rider picks a route" $ resolveCode "9999" ["ML05A9999", "ML07C9999"] @?= ManyCabs,
          testCase "one cab on two routes is still one cab" $ resolveCode "9999" ["ML05A9999", "ML05A9999"] @?= OneCab "ML05A9999",
          testCase "a bus the fleet lookup knows by a four-digit code still resolves to the bus, even if cabs end in it" $
            [chooseWalkUp True (OneCab "ML05A9999"), chooseWalkUp True ManyCabs, chooseWalkUp True NoCab] @?= [UseBus, UseBus, UseBus],
          testCase "an unknown number (the bus path answers with the whole feed, no vehicle found) resolves to the one cab" $
            chooseWalkUp False (OneCab "ML05A9999") @?= UseCab "ML05A9999",
          testCase "several cabs, and no bus vehicle: the route picker; no cab either: the bus path's own answer" $
            [chooseWalkUp False ManyCabs, chooseWalkUp False NoCab] @?= [PickRoute, UseBus],
          testCase "serviceability answers from the cabs only for routes of a readable shared-cab feed" $
            [cabRouteRequest (Just ["SC-A-F", "SC-A-R"]) ["SC-A-F"], cabRouteRequest (Just ["SC-A-F"]) ["SC-A-F", "MTC-1"], cabRouteRequest (Just ["SC-A-F"]) []] @?= [True, False, False],
          testCase "a failed or absent feed read (OTP down, no shared-cab feed) leaves the request on the bus path" $
            cabRouteRequest Nothing ["SC-A-F"] @?= False,
          testCase "the bus probe passes a found vehicle on, reports an unknown number as Nothing, and counts a failed lookup as not found (so a transient error can let exactly one live cab win, but never fails the walk-up)" $ do
            found <- probeBus (pure (Just ("bus", 1 :: Int)))
            unknown <- probeBus (pure (Nothing :: Maybe (String, Int)))
            failed <- probeBus (ioError (userError "nandi down") :: IO (Maybe (String, Int)))
            (found, unknown, failed) @?= (Just ("bus", 1), Nothing, Nothing),
          testCase "four digits only" $ map isStickerCode ["9999", "ML05", "99999", "999", ""] @?= [True, False, False, False, False]
        ]
    ]
