{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabWalkUpTests (tests) where

import qualified "rider-app" API.Types.UI.FRFSTicketService as ApiFRFS
import qualified "rider-app" API.Types.UI.MultimodalConfirm as ApiTypes
import qualified BecknV2.FRFS.Enums as Spec
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Time (UTCTime (..), fromGregorian)
import "mobility-core" Kernel.External.Maps.Types (LatLong (..))
import "mobility-core" Kernel.Types.Id (Id (..))
import "rider-app" SharedLogic.SharedCab.WalkUp (CodeResolution (..), cabRouteServiceTier, cabRouteStations, cabRouteView, declaresSharedCab, enabledCabConfig, isSharedCabTier, isStickerCode, passingCab, resolveCode, stopPairLegs, withoutCabRows)
import qualified "rider-app" Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "SharedCab walk-up"
    [ testCase "a cab approaching a stop shows only its plate's last four" $
        (passingCab "ML05A9999" (CQMMB.BusStopETA "SC_STOP" Nothing (UTCTime (fromGregorian 2026 10 28) 0) 0 Nothing)).vehicleNumber @?= "9999",
      testGroup
        "walk-up: a typed sticker code resolves among the live cabs"
        [ testCase "exactly one cab ends in the code" $ resolveCode "9999" ["ML05A9999", "ML05B8888"] @?= OneCab "ML05A9999",
          testCase "no cab ends in the code" $ resolveCode "1234" ["ML05A9999", "ML05B8888"] @?= NoCab,
          testCase "two cabs end in the code: the rider picks a route" $ resolveCode "9999" ["ML05A9999", "ML07C9999"] @?= ManyCabs,
          testCase "one cab on two routes is still one cab" $ resolveCode "9999" ["ML05A9999", "ML05A9999"] @?= OneCab "ML05A9999",
          testCase "a vehicle lookup takes the cab path only when the client asks for the SHARED_CAB tier; every other lookup is the bus path" $
            map declaresSharedCab [Just [Spec.SHARED_CAB], Just [Spec.PREMIUM, Spec.SHARED_CAB], Just [Spec.PREMIUM, Spec.SHUTTLE], Just [], Nothing]
              @?= [True, True, False, False, False],
          testCase "route serviceability and a stop's passing routes take the cab path only for the SHARED_CAB tier; anything else is the bus path" $
            map isSharedCabTier [Just Spec.SHARED_CAB, Just Spec.PREMIUM, Just Spec.ORDINARY, Nothing] @?= [True, False, False, False],
          testCase "a cab request with stop codes and no route asks for the cab feed's routes at the stops; none there is no leg" $
            map
              (fmap (map (\ApiTypes.RouteCodesWithLeg {ApiTypes.legOrder = o, ApiTypes.routeCodes = c} -> (o, c))))
              [ stopPairLegs (Just "SC_POLO") ["SC-PRD-F"],
                stopPairLegs (Just "SC_POLO") [],
                stopPairLegs Nothing ["SC-PRD-F"]
              ]
              @?= [Just [(0, ["SC-PRD-F"])], Nothing, Nothing],
          testCase "a city without a cab config never reads the shared-cab switch; one with it is answered by the switch" $ do
            switchReads <- newIORef (0 :: Int)
            let switch on = modifyIORef switchReads (+ 1) >> pure on
            found <- sequence [enabledCabConfig (switch True) (Nothing :: Maybe Int), enabledCabConfig (switch True) (Just 1), enabledCabConfig (switch False) (Just 1)]
            readIORef switchReads >>= \n -> (found, n) @?= ([Nothing, Just 1, Nothing], 2),
          testCase "the bus fleet's configs never include the shared-cab feed" $
            withoutCabRows fst [("chennai_data_bus", 1 :: Int), ("shillong_shared_cab:SHARED_CAB", 2)] @?= [("chennai_data_bus", 1)],
          testCase "a shared-cab route advertises SHARED_CAB only to a client that declared the tier; other routes and clients are untouched" $
            [ cabRouteServiceTier True (Just [Spec.PREMIUM, Spec.SHUTTLE, Spec.SHARED_CAB]),
              cabRouteServiceTier True (Just [Spec.PREMIUM, Spec.SHUTTLE]),
              cabRouteServiceTier True Nothing,
              cabRouteServiceTier False (Just [Spec.SHARED_CAB])
            ]
              @?= [Just Spec.SHARED_CAB, Nothing, Nothing, Nothing],
          testCase "a cab route's stops come back in stop order with their points and the route's code" $
            map
              (\ApiFRFS.FRFSStationAPI {ApiFRFS.code = c, ApiFRFS.sequenceNum = n, ApiFRFS.lat = la, ApiFRFS.routeCodes = rc} -> (c, n, la, rc))
              (cabRouteStations (Id "ibc") "SC-A-F" Nothing "A to C" [("C", "Charlie", 3, LatLong 25.3 91.3), ("A", "Alpha", 1, LatLong 25.1 91.1), ("B", "Bravo", 2, LatLong 25.2 91.2)])
              @?= [("A", Just 1, Just 25.1, Just ["SC-A-F"]), ("B", Just 2, Just 25.2, Just ["SC-A-F"]), ("C", Just 3, Just 25.3, Just ["SC-A-F"])],
          testCase "a cab route's shape is its polyline when it has one, else its stops in order, and nothing with neither" $
            let stops = [("B", "Bravo", 2, LatLong 25.2 91.2), ("A", "Alpha", 1, LatLong 25.1 91.1)]
                shape s p = snd (cabRouteView (Id "ibc") "SC-A-B" Nothing "A to B" s p)
             in [shape stops (Just [LatLong 1 1]), shape stops Nothing, shape [] Nothing] @?= [Just [LatLong 1 1], Just [LatLong 25.1 91.1, LatLong 25.2 91.2], Nothing],
          testCase "four digits only" $ map isStickerCode ["9999", "ML05", "99999", "999", ""] @?= [True, False, False, False, False]
        ]
    ]
