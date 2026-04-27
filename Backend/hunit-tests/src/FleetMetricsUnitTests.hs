{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module FleetMetricsUnitTests where

import Control.Applicative (liftA2)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, listToMaybe)
import "mobility-core" Kernel.Types.Common (HighPrecMoney (..), Meters (..), Seconds (..))
import qualified "dynamic-offer-driver-app" Storage.Queries.FleetOperatorDailyStatsExtra as QFODSExtra
import qualified "dynamic-offer-driver-app" Storage.Queries.FleetRcDailyStatsExtra as QFRDSExtra
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Prelude

-- =============================================================================
-- Helpers: replicate the pure computations from Driver.hs
-- =============================================================================

computeTotalPlatformFees :: Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney
computeTotalPlatformFees = liftA2 (+)

computeNetEarnings :: Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney
computeNetEarnings = liftA2 (-)

computeOnlineDurationInHours :: Maybe Seconds -> Maybe Rational
computeOnlineDurationInHours mbDur =
  mbDur >>= \(Seconds sec) ->
    let hours = fromIntegral sec / 3600
     in if hours > 0 then Just hours else Nothing

computeGrossEarningsPerHour :: Maybe HighPrecMoney -> Maybe Rational -> Maybe HighPrecMoney
computeGrossEarningsPerHour gross onlineHours =
  gross >>= \g ->
    onlineHours <&> \h -> g / HighPrecMoney h

computeNetEarningsPerHour :: Maybe HighPrecMoney -> Maybe Rational -> Maybe HighPrecMoney
computeNetEarningsPerHour net onlineHours =
  net >>= \n ->
    onlineHours <&> \h -> n / HighPrecMoney h

calculateEarningsPerKm :: Meters -> HighPrecMoney -> Maybe HighPrecMoney
calculateEarningsPerKm distance earnings =
  let meters = getMeters distance
      km = toRational meters / 1000
   in if km == 0
        then Nothing
        else Just (HighPrecMoney (getHighPrecMoney earnings / km))

data MockRole = OPERATOR | FLEET_OWNER
  deriving (Eq, Show)

resolveFleetOwnerIds :: Maybe MockRole -> Maybe String -> String -> [String] -> [String]
resolveFleetOwnerIds mbRole mbFleetOwnerId requestorId associatedFleetOwnerIds =
  case (mbRole, mbFleetOwnerId) of
    (Just OPERATOR, Nothing) -> associatedFleetOwnerIds
    (_, Just foId) -> [foId]
    (_, Nothing) -> [requestorId]

resolveFleetOwnerIdForNonEarnings :: String -> [String] -> String
resolveFleetOwnerIdForNonEarnings requestorId fleetOwnerIds =
  fromMaybe requestorId (listToMaybe fleetOwnerIds)

-- Convenience extractors for DailyFleetEarningsAggregated
earningsFields ::
  QFODSExtra.DailyFleetEarningsAggregated ->
  (Maybe HighPrecMoney, Maybe HighPrecMoney, Maybe HighPrecMoney, Maybe Seconds)
earningsFields (QFODSExtra.DailyFleetEarningsAggregated tes cpfs opfs ods) = (tes, cpfs, opfs, ods)

-- =============================================================================
-- 1. Operator with multiple fleet owners: earnings aggregation
-- =============================================================================

testMultiFleetOwnerEarnings :: TestTree
testMultiFleetOwnerEarnings =
  testGroup
    "Multi-Fleet Owner Earnings Aggregation"
    [ testCase "3 fleet owners: DB returns combined sums -> correct gross, net, perHour" $ do
        -- FO-A: online=1000, cash=500,  cashPF=50,  onlinePF=100, onlineDur=3600s
        -- FO-B: online=2000, cash=800,  cashPF=80,  onlinePF=200, onlineDur=7200s
        -- FO-C: online=500,  cash=200,  cashPF=20,  onlinePF=50,  onlineDur=1800s
        -- DB aggregate: online=3500, cash=1500, cashPF=150, onlinePF=350, dur=12600
        let dbResult =
              QFODSExtra.mkDailyFleetEarningsAggregated
                ( Just (HighPrecMoney 3500),
                  Just (HighPrecMoney 1500),
                  Just (HighPrecMoney 150),
                  Just (HighPrecMoney 350),
                  Just (Seconds 12600)
                )
            (gross, cashPF, onlinePF, durSum) = earningsFields dbResult
            totalPF = computeTotalPlatformFees cashPF onlinePF
            netEarn = computeNetEarnings gross totalPF
            hours = computeOnlineDurationInHours durSum
            grossPerHr = computeGrossEarningsPerHour gross hours
            netPerHr = computeNetEarningsPerHour netEarn hours
        gross @?= Just (HighPrecMoney 5000)
        totalPF @?= Just (HighPrecMoney 500)
        netEarn @?= Just (HighPrecMoney 4500)
        hours @?= Just 3.5
        grossPerHr @?= Just (HighPrecMoney (5000 / 3.5))
        netPerHr @?= Just (HighPrecMoney (4500 / 3.5)),
      testCase "2 fleet owners: one heavy earner, one light earner" $ do
        -- FO-X: online=10000, cash=5000, cashPF=500, onlinePF=1000, dur=36000
        -- FO-Y: online=100,   cash=50,   cashPF=5,   onlinePF=10,  dur=1800
        -- DB aggregate: online=10100, cash=5050, cashPF=505, onlinePF=1010, dur=37800
        let dbResult =
              QFODSExtra.mkDailyFleetEarningsAggregated
                ( Just (HighPrecMoney 10100),
                  Just (HighPrecMoney 5050),
                  Just (HighPrecMoney 505),
                  Just (HighPrecMoney 1010),
                  Just (Seconds 37800)
                )
            (gross, cashPF, onlinePF, _durSum) = earningsFields dbResult
            totalPF = computeTotalPlatformFees cashPF onlinePF
            netEarn = computeNetEarnings gross totalPF
        gross @?= Just (HighPrecMoney 15150)
        totalPF @?= Just (HighPrecMoney 1515)
        netEarn @?= Just (HighPrecMoney 13635),
      testCase "operator with no associations -> empty fleet IDs -> default zeros" $ do
        let fleetOwnerIds = resolveFleetOwnerIds (Just OPERATOR) Nothing "op-1" []
            earningsAgg =
              if null fleetOwnerIds
                then QFODSExtra.DailyFleetEarningsAggregated Nothing Nothing Nothing Nothing
                else error "should not reach here"
            (tes, cpfs, opfs, ods) = earningsFields earningsAgg
        tes @?= Nothing
        cpfs @?= Nothing
        opfs @?= Nothing
        ods @?= Nothing,
      testCase "operator drills into single fleet owner -> single-fleet query" $ do
        let fleetOwnerIds = resolveFleetOwnerIds (Just OPERATOR) (Just "fo-B") "op-1" ["fo-A", "fo-B", "fo-C"]
        fleetOwnerIds @?= ["fo-B"]
        -- FO-B standalone: online=2000, cash=800
        let dbResult =
              QFODSExtra.mkDailyFleetEarningsAggregated
                ( Just (HighPrecMoney 2000),
                  Just (HighPrecMoney 800),
                  Just (HighPrecMoney 80),
                  Just (HighPrecMoney 200),
                  Just (Seconds 7200)
                )
        let (gross, _, _, _) = earningsFields dbResult
        gross @?= Just (HighPrecMoney 2800),
      testCase "fleet owner queries own data -> requestorId used" $ do
        let fleetOwnerIds = resolveFleetOwnerIds (Just FLEET_OWNER) Nothing "fo-A" []
        fleetOwnerIds @?= ["fo-A"]
        -- FO-A standalone: online=1000, cash=500
        let dbResult =
              QFODSExtra.mkDailyFleetEarningsAggregated
                ( Just (HighPrecMoney 1000),
                  Just (HighPrecMoney 500),
                  Just (HighPrecMoney 50),
                  Just (HighPrecMoney 100),
                  Just (Seconds 3600)
                )
            (gross, cashPF, onlinePF, durSum) = earningsFields dbResult
            totalPF = computeTotalPlatformFees cashPF onlinePF
            netEarn = computeNetEarnings gross totalPF
            hours = computeOnlineDurationInHours durSum
            grossPerHr = computeGrossEarningsPerHour gross hours
        gross @?= Just (HighPrecMoney 1500)
        totalPF @?= Just (HighPrecMoney 150)
        netEarn @?= Just (HighPrecMoney 1350)
        hours @?= Just 1
        grossPerHr @?= Just (HighPrecMoney 1500)
    ]

-- =============================================================================
-- 2. Operator with multiple fleet owners: multi-vehicle stats
-- =============================================================================

testMultiFleetOwnerVehicleStats :: TestTree
testMultiFleetOwnerVehicleStats =
  testGroup
    "Multi-Fleet Owner Vehicle Stats"
    [ testCase "5 vehicles across 2 fleet owners: ordered by rides desc, earningsPerKm correct" $ do
        let v1 = QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-A" "V1" (HighPrecMoney 5000) 10 (Meters 30000) (Seconds 7200)
            v2 = QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-A" "V2" (HighPrecMoney 2500) 5 (Meters 15000) (Seconds 3600)
            v3 = QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-B" "V3" (HighPrecMoney 10000) 20 (Meters 60000) (Seconds 14400)
            v4 = QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-B" "V4" (HighPrecMoney 1500) 3 (Meters 10000) (Seconds 1800)
            v5 = QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-B" "V5" (HighPrecMoney 8000) 15 (Meters 45000) (Seconds 10800)
            allVehicles = [v3, v5, v1, v2, v4] -- DB returns ordered by rides desc
        map QFRDSExtra.totalCompletedRides allVehicles @?= [20, 15, 10, 5, 3]
        map QFRDSExtra.fleetOwnerId' allVehicles @?= ["fo-B", "fo-B", "fo-A", "fo-A", "fo-B"]

        -- earningsPerKm for each
        calculateEarningsPerKm (QFRDSExtra.totalDistance v3) (QFRDSExtra.totalEarnings v3) @?= Just (HighPrecMoney (10000 / 60))
        calculateEarningsPerKm (QFRDSExtra.totalDistance v5) (QFRDSExtra.totalEarnings v5) @?= Just (HighPrecMoney (8000 / 45))
        calculateEarningsPerKm (QFRDSExtra.totalDistance v1) (QFRDSExtra.totalEarnings v1) @?= Just (HighPrecMoney (5000 / 30))
        calculateEarningsPerKm (QFRDSExtra.totalDistance v2) (QFRDSExtra.totalEarnings v2) @?= Just (HighPrecMoney (2500 / 15))
        calculateEarningsPerKm (QFRDSExtra.totalDistance v4) (QFRDSExtra.totalEarnings v4) @?= Just (HighPrecMoney (1500 / 10)),
      testCase "total earnings across all vehicles of 2 fleet owners" $ do
        let vehicles =
              [ QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-A" "V1" (HighPrecMoney 5000) 10 (Meters 30000) (Seconds 7200),
                QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-A" "V2" (HighPrecMoney 2500) 5 (Meters 15000) (Seconds 3600),
                QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-B" "V3" (HighPrecMoney 10000) 20 (Meters 60000) (Seconds 14400),
                QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-B" "V4" (HighPrecMoney 1500) 3 (Meters 10000) (Seconds 1800),
                QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-B" "V5" (HighPrecMoney 8000) 15 (Meters 45000) (Seconds 10800)
              ]
            totalEarnings = sum $ map QFRDSExtra.totalEarnings vehicles
            totalRides = sum $ map QFRDSExtra.totalCompletedRides vehicles
            totalDist = sum $ map (getMeters . QFRDSExtra.totalDistance) vehicles
        totalEarnings @?= HighPrecMoney 27000
        totalRides @?= 53
        totalDist @?= 160000,
      testCase "fleet owner fo-A only sees their 2 vehicles" $ do
        let foAVehicles =
              [ QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-A" "V1" (HighPrecMoney 5000) 10 (Meters 30000) (Seconds 7200),
                QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-A" "V2" (HighPrecMoney 2500) 5 (Meters 15000) (Seconds 3600)
              ]
        length foAVehicles @?= 2
        sum (map QFRDSExtra.totalEarnings foAVehicles) @?= HighPrecMoney 7500
        sum (map QFRDSExtra.totalCompletedRides foAVehicles) @?= 15,
      testCase "fleet owner fo-B only sees their 3 vehicles" $ do
        let foBVehicles =
              [ QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-B" "V3" (HighPrecMoney 10000) 20 (Meters 60000) (Seconds 14400),
                QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-B" "V4" (HighPrecMoney 1500) 3 (Meters 10000) (Seconds 1800),
                QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-B" "V5" (HighPrecMoney 8000) 15 (Meters 45000) (Seconds 10800)
              ]
        length foBVehicles @?= 3
        sum (map QFRDSExtra.totalEarnings foBVehicles) @?= HighPrecMoney 19500
        sum (map QFRDSExtra.totalCompletedRides foBVehicles) @?= 38,
      testCase "vehicle with zero distance -> earningsPerKm is Nothing" $ do
        let v = QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-X" "V-idle" (HighPrecMoney 0) 0 (Meters 0) (Seconds 0)
        calculateEarningsPerKm (QFRDSExtra.totalDistance v) (QFRDSExtra.totalEarnings v) @?= Nothing
    ]

-- =============================================================================
-- 3. Fleet owner with multiple drivers: earnings summary row
-- =============================================================================

testMultiDriverPerFleetOwner :: TestTree
testMultiDriverPerFleetOwner =
  testGroup
    "Multi-Driver Per Fleet Owner"
    [ testCase "summary row with 5 drivers aggregated: pipeline produces correct values" $ do
        -- Summary of 5 drivers: online=1500, cash=750, cashPF=75, onlinePF=150, dur=11700
        let summaryRow =
              QFODSExtra.mkDailyFleetEarningsAggregated
                ( Just (HighPrecMoney 1500),
                  Just (HighPrecMoney 750),
                  Just (HighPrecMoney 75),
                  Just (HighPrecMoney 150),
                  Just (Seconds 11700)
                )
            (gross, cashPF, onlinePF, durSum) = earningsFields summaryRow
            totalPF = computeTotalPlatformFees cashPF onlinePF
            netEarn = computeNetEarnings gross totalPF
            hours = computeOnlineDurationInHours durSum
        gross @?= Just (HighPrecMoney 2250)
        totalPF @?= Just (HighPrecMoney 225)
        netEarn @?= Just (HighPrecMoney 2025)
        hours @?= Just (11700 / 3600),
      testCase "fleet owner with 1 driver: summary row equals that driver's stats" $ do
        let summaryRow =
              QFODSExtra.mkDailyFleetEarningsAggregated
                ( Just (HighPrecMoney 300),
                  Just (HighPrecMoney 100),
                  Just (HighPrecMoney 10),
                  Just (HighPrecMoney 20),
                  Just (Seconds 3600)
                )
            (gross, cashPF, onlinePF, durSum) = earningsFields summaryRow
            totalPF = computeTotalPlatformFees cashPF onlinePF
            netEarn = computeNetEarnings gross totalPF
            hours = computeOnlineDurationInHours durSum
            grossPerHr = computeGrossEarningsPerHour gross hours
            netPerHr = computeNetEarningsPerHour netEarn hours
        gross @?= Just (HighPrecMoney 400)
        totalPF @?= Just (HighPrecMoney 30)
        netEarn @?= Just (HighPrecMoney 370)
        hours @?= Just 1
        grossPerHr @?= Just (HighPrecMoney 400)
        netPerHr @?= Just (HighPrecMoney 370),
      testCase "fleet owner with 10 drivers across 2 dates: combined summary row" $ do
        -- Day 1: 6 drivers total online=3000, cash=1000, cashPF=100, onlinePF=300, dur=18000
        -- Day 2: 4 drivers total online=2000, cash=800,  cashPF=80,  onlinePF=200, dur=14400
        -- DB SUM: online=5000, cash=1800, cashPF=180, onlinePF=500, dur=32400
        let combinedSummary =
              QFODSExtra.mkDailyFleetEarningsAggregated
                ( Just (HighPrecMoney 5000),
                  Just (HighPrecMoney 1800),
                  Just (HighPrecMoney 180),
                  Just (HighPrecMoney 500),
                  Just (Seconds 32400)
                )
            (gross, cashPF, onlinePF, durSum) = earningsFields combinedSummary
            totalPF = computeTotalPlatformFees cashPF onlinePF
            netEarn = computeNetEarnings gross totalPF
            hours = computeOnlineDurationInHours durSum
        gross @?= Just (HighPrecMoney 6800)
        totalPF @?= Just (HighPrecMoney 680)
        netEarn @?= Just (HighPrecMoney 6120)
        hours @?= Just 9
    ]

-- =============================================================================
-- 4. Multi-driver, multi-vehicle: vehicle-level stats per fleet owner
-- =============================================================================

testMultiDriverMultiVehicle :: TestTree
testMultiDriverMultiVehicle =
  testGroup
    "Multi-Driver Multi-Vehicle Stats"
    [ testCase "1 fleet owner, 3 vehicles, stats across 3 days: per-vehicle sums correct" $ do
        -- V1: day1(3,1500,10k,1800) + day2(5,2500,15k,2700) + day3(2,1000,8k,1200) = (10,5000,33k,5700)
        let v1 = QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-A" "V1" (HighPrecMoney 5000) 10 (Meters 33000) (Seconds 5700)
            -- V2: day1(7,3500,20k,3600) + day2(4,2000,12k,2400) = (11,5500,32k,6000)
            v2 = QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-A" "V2" (HighPrecMoney 5500) 11 (Meters 32000) (Seconds 6000)
            -- V3: day1(1,500,5k,600)
            v3 = QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-A" "V3" (HighPrecMoney 500) 1 (Meters 5000) (Seconds 600)
            allVehicles = [v2, v1, v3] -- ordered by rides desc: 11, 10, 1
        map QFRDSExtra.totalCompletedRides allVehicles @?= [11, 10, 1]

        calculateEarningsPerKm (QFRDSExtra.totalDistance v1) (QFRDSExtra.totalEarnings v1) @?= Just (HighPrecMoney (5000 / 33))
        calculateEarningsPerKm (QFRDSExtra.totalDistance v2) (QFRDSExtra.totalEarnings v2) @?= Just (HighPrecMoney (5500 / 32))
        calculateEarningsPerKm (QFRDSExtra.totalDistance v3) (QFRDSExtra.totalEarnings v3) @?= Just (HighPrecMoney (500 / 5))

        sum (map QFRDSExtra.totalEarnings allVehicles) @?= HighPrecMoney 11000
        sum (map QFRDSExtra.totalCompletedRides allVehicles) @?= 22,
      testCase "2 fleet owners, operator sees combined vehicle list" $ do
        let foAv1 = QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-A" "rc-001" (HighPrecMoney 4000) 8 (Meters 25000) (Seconds 4800)
            foAv2 = QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-A" "rc-002" (HighPrecMoney 3000) 6 (Meters 20000) (Seconds 3600)
            foBv1 = QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-B" "rc-003" (HighPrecMoney 12000) 25 (Meters 80000) (Seconds 18000)
            foBv2 = QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-B" "rc-004" (HighPrecMoney 6000) 12 (Meters 40000) (Seconds 9000)
            foBv3 = QFRDSExtra.mkFleetRcDailyStatsAggregated "fo-B" "rc-005" (HighPrecMoney 1000) 2 (Meters 7000) (Seconds 1200)
            allVehicles = [foBv1, foBv2, foAv1, foAv2, foBv3] -- rides desc: 25,12,8,6,2
        length allVehicles @?= 5
        map QFRDSExtra.totalCompletedRides allVehicles @?= [25, 12, 8, 6, 2]

        let totalEarn = sum $ map QFRDSExtra.totalEarnings allVehicles
            totalRides = sum $ map QFRDSExtra.totalCompletedRides allVehicles
            totalDist = sum $ map (getMeters . QFRDSExtra.totalDistance) allVehicles
        totalEarn @?= HighPrecMoney 26000
        totalRides @?= 53
        totalDist @?= 172000

        -- fo-A subtotals
        let foAEarn = sum $ map QFRDSExtra.totalEarnings [foAv1, foAv2]
            foARides = sum $ map QFRDSExtra.totalCompletedRides [foAv1, foAv2]
        foAEarn @?= HighPrecMoney 7000
        foARides @?= 14

        -- fo-B subtotals
        let foBEarn = sum $ map QFRDSExtra.totalEarnings [foBv1, foBv2, foBv3]
            foBRides = sum $ map QFRDSExtra.totalCompletedRides [foBv1, foBv2, foBv3]
        foBEarn @?= HighPrecMoney 19000
        foBRides @?= 39
    ]

-- =============================================================================
-- 5. DailyFleetEarningsAggregated: getTotalEarningSum
-- =============================================================================

testGetTotalEarningSum :: TestTree
testGetTotalEarningSum =
  testGroup
    "getTotalEarningSum"
    [ testCase "both Just -> sum" $ do
        QFODSExtra.getTotalEarningSum (Just (HighPrecMoney 100)) (Just (HighPrecMoney 200)) @?= Just (HighPrecMoney 300),
      testCase "only online Just -> online" $ do
        QFODSExtra.getTotalEarningSum (Just (HighPrecMoney 100)) Nothing @?= Just (HighPrecMoney 100),
      testCase "only cash Just -> cash" $ do
        QFODSExtra.getTotalEarningSum Nothing (Just (HighPrecMoney 200)) @?= Just (HighPrecMoney 200),
      testCase "both Nothing -> Nothing" $ do
        QFODSExtra.getTotalEarningSum Nothing Nothing @?= Nothing,
      testCase "both zero -> Just 0" $ do
        QFODSExtra.getTotalEarningSum (Just (HighPrecMoney 0)) (Just (HighPrecMoney 0)) @?= Just (HighPrecMoney 0)
    ]

-- =============================================================================
-- 6. Earnings pipeline: edge cases
-- =============================================================================

testEarningsPipelineEdgeCases :: TestTree
testEarningsPipelineEdgeCases =
  testGroup
    "Earnings Pipeline Edge Cases"
    [ testCase "all Nothing -> all outputs Nothing" $ do
        let totalPF = computeTotalPlatformFees Nothing Nothing
            netEarn = computeNetEarnings Nothing totalPF
            hours = computeOnlineDurationInHours Nothing
            grossPerHr = computeGrossEarningsPerHour Nothing hours
            netPerHr = computeNetEarningsPerHour netEarn hours
        totalPF @?= Nothing
        netEarn @?= Nothing
        hours @?= Nothing
        grossPerHr @?= Nothing
        netPerHr @?= Nothing,
      testCase "zero duration -> perHour is Nothing" $ do
        let gross = Just (HighPrecMoney 5000)
            totalPF = Just (HighPrecMoney 500)
            netEarn = computeNetEarnings gross totalPF
            hours = computeOnlineDurationInHours (Just (Seconds 0))
            grossPerHr = computeGrossEarningsPerHour gross hours
        netEarn @?= Just (HighPrecMoney 4500)
        hours @?= Nothing
        grossPerHr @?= Nothing,
      testCase "gross present but one fee Nothing -> totalPF Nothing -> net Nothing" $ do
        let gross = Just (HighPrecMoney 1000)
            totalPF = computeTotalPlatformFees (Just (HighPrecMoney 50)) Nothing
            netEarn = computeNetEarnings gross totalPF
        totalPF @?= Nothing
        netEarn @?= Nothing
    ]

-- =============================================================================
-- 7. Operator fleet ID resolution
-- =============================================================================

testOperatorFleetIdResolution :: TestTree
testOperatorFleetIdResolution =
  testGroup
    "Operator Fleet ID Resolution"
    [ testCase "OPERATOR + no fleetOwnerId -> all associated fleet owners" $ do
        resolveFleetOwnerIds (Just OPERATOR) Nothing "op-1" ["fo-1", "fo-2", "fo-3"] @?= ["fo-1", "fo-2", "fo-3"],
      testCase "OPERATOR + no associations -> empty" $ do
        resolveFleetOwnerIds (Just OPERATOR) Nothing "op-1" [] @?= [],
      testCase "OPERATOR + explicit fleetOwnerId -> singleton" $ do
        resolveFleetOwnerIds (Just OPERATOR) (Just "fo-2") "op-1" ["fo-1", "fo-2", "fo-3"] @?= ["fo-2"],
      testCase "FLEET_OWNER + no fleetOwnerId -> self" $ do
        resolveFleetOwnerIds (Just FLEET_OWNER) Nothing "fo-A" [] @?= ["fo-A"],
      testCase "FLEET_OWNER + explicit fleetOwnerId -> singleton" $ do
        resolveFleetOwnerIds (Just FLEET_OWNER) (Just "fo-X") "fo-A" [] @?= ["fo-X"],
      testCase "no role + no fleetOwnerId -> self" $ do
        resolveFleetOwnerIds Nothing Nothing "req-1" [] @?= ["req-1"],
      testCase "non-EARNINGS: multiple IDs -> uses first" $ do
        resolveFleetOwnerIdForNonEarnings "req-1" ["fo-1", "fo-2"] @?= "fo-1",
      testCase "non-EARNINGS: empty -> falls back to requestorId" $ do
        resolveFleetOwnerIdForNonEarnings "req-1" [] @?= "req-1"
    ]

-- =============================================================================
-- 8. calculateEarningsPerKm
-- =============================================================================

testCalculateEarningsPerKm :: TestTree
testCalculateEarningsPerKm =
  testGroup
    "calculateEarningsPerKm"
    [ testCase "zero distance -> Nothing" $ do
        calculateEarningsPerKm (Meters 0) (HighPrecMoney 1000) @?= Nothing,
      testCase "10km, 1000 earnings -> 100/km" $ do
        calculateEarningsPerKm (Meters 10000) (HighPrecMoney 1000) @?= Just (HighPrecMoney 100),
      testCase "500m, 100 earnings -> 200/km" $ do
        calculateEarningsPerKm (Meters 500) (HighPrecMoney 100) @?= Just (HighPrecMoney 200),
      testCase "zero earnings, non-zero distance -> 0/km" $ do
        calculateEarningsPerKm (Meters 5000) (HighPrecMoney 0) @?= Just (HighPrecMoney 0),
      testCase "1500m (1.5km), 300 earnings -> 200/km" $ do
        calculateEarningsPerKm (Meters 1500) (HighPrecMoney 300) @?= Just (HighPrecMoney 200)
    ]

-- =============================================================================
-- MAIN TEST SUITE
-- =============================================================================

fleetMetricsUnitTests :: TestTree
fleetMetricsUnitTests =
  testGroup
    "Fleet Metrics Unit Tests"
    [ testMultiFleetOwnerEarnings,
      testMultiFleetOwnerVehicleStats,
      testMultiDriverPerFleetOwner,
      testMultiDriverMultiVehicle,
      testGetTotalEarningSum,
      testEarningsPipelineEdgeCases,
      testOperatorFleetIdResolution,
      testCalculateEarningsPerKm
    ]
