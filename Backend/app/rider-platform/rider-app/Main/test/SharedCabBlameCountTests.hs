{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | R18: the "which counter to bump" decision (SharedLogic.SharedCab.BlameCount.subjectFor) is the
-- only branch in the bump path -- everything else is plumbing (one atomic upsert). Falsified by
-- swapping the outcome BlameRider/BlameDriver arm and watching the wrong counter light up.
module SharedCabBlameCountTests (tests) where

import qualified "rider-app" Domain.Types.Merchant as DM
import qualified Domain.Types.SharedCabBlameCount as DBlame
import Kernel.Prelude
import Kernel.Types.Id
import SharedLogic.SharedCab.Allocation.Types (Blame (..))
import SharedLogic.SharedCab.BlameCount (subjectFor)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

riderMerchant, driverMerchant :: Id DM.Merchant
riderMerchant = Id "rider-merchant"
driverMerchant = Id "driver-merchant"

rider :: (Text, Id DM.Merchant)
rider = ("rider-1", riderMerchant)

driver :: (Text, Id DM.Merchant)
driver = ("driver-1", driverMerchant)

tests :: TestTree
tests =
  testGroup
    "SharedCab blame count (R18 05 §8.4)"
    [ testCase "BlameRider charges the rider's no-show counter, not the driver's" $
        subjectFor BlameRider (Just rider) (Just driver) @?= Just (DBlame.RIDER_NO_SHOW, "rider-1", riderMerchant),
      testCase "BlameDriver charges the plate's session driver, not the rider" $
        subjectFor BlameDriver (Just rider) (Just driver) @?= Just (DBlame.DRIVER_MISS, "driver-1", driverMerchant),
      testCase "BlameNone bumps nothing, even with both a rider and a driver in hand" $
        subjectFor BlameNone (Just rider) (Just driver) @?= Nothing,
      testCase "BlameRider with no booking row bumps nothing (never silently blames the driver instead)" $
        subjectFor BlameRider Nothing (Just driver) @?= Nothing,
      testCase "BlameDriver with no live session bumps nothing (never silently blames the rider instead)" $
        subjectFor BlameDriver (Just rider) Nothing @?= Nothing
    ]
