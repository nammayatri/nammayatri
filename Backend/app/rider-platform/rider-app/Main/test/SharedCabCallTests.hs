{-# LANGUAGE PackageImports #-}

module SharedCabCallTests (tests) where

import "rider-app" Domain.Types.RiderConfig (SharedCabCallMode (..))
import "rider-app" SharedLogic.SharedCab.Call (riderCallTarget)
import "rider-app" SharedLogic.SharedCab.Config (SharedCabTunables (..), activeCallMode, defaultTunables)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

data Ride = Ride deriving (Eq, Show)

data Cab = Cab deriving (Eq, Show)

target :: Maybe Bool -> Maybe Bool -> Maybe (Either Ride Cab)
target ride cab = riderCallTarget ((Ride,) <$> ride) ((Cab,) <$> cab)

tests :: TestTree
tests = testGroup "shared-cab calls" [callTargetTests, callModeTests]

callModeTests :: TestTree
callModeTests =
  testGroup
    "how rider and driver reach each other"
    [ testCase "calls off: no number either way, whatever the mode" $
        [activeCallMode defaultTunables {callEnabled = False, callMode = m} | m <- [MASKED, DIRECT]] @?= [Nothing, Nothing],
      testCase "calls on: the city's mode" $
        [activeCallMode defaultTunables {callEnabled = True, callMode = m} | m <- [MASKED, DIRECT]] @?= [Just MASKED, Just DIRECT],
      testCase "a city that only turns calls on gets masked calls" $
        activeCallMode defaultTunables {callEnabled = True} @?= Just MASKED
    ]

callTargetTests :: TestTree
callTargetTests =
  testGroup
    "whose driver a rider's call to the city ExoPhone reaches"
    [ testCase "a rider with a taxi and a live cab who dials the cab's ExoPhone reaches the cab" $
        target (Just False) (Just True) @?= Just (Right Cab),
      testCase "both flows handed out the dialled ExoPhone: the live cab owns the call" $
        target (Just True) (Just True) @?= Just (Right Cab),
      testCase "the dialled ExoPhone is the taxi's and not the cab's: the taxi" $
        target (Just True) (Just False) @?= Just (Left Ride),
      testCase "a number neither handed out goes to the live cab" $
        target (Just False) (Just False) @?= Just (Right Cab),
      testCase "only a cab, only a taxi, or neither: as before, whatever was dialled" $
        [target Nothing (Just b) | b <- [True, False]] ++ [target (Just b) Nothing | b <- [True, False]] ++ [target Nothing Nothing]
          @?= [Just (Right Cab), Just (Right Cab), Just (Left Ride), Just (Left Ride), Nothing]
    ]
