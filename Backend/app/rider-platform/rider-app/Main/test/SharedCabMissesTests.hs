{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | R18: the "who gets charged" decision (SharedLogic.SharedCab.Misses.chargeFor) is the only branch in the
-- counter path -- everything else is plumbing (one atomic increment). Falsified by swapping the BlameRider /
-- BlameDriver arms, and by dropping the driver-match guard, and watching the wrong counter light up.
module SharedCabMissesTests (tests) where

import qualified "rider-app" Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.VehicleTrip as DVT
import Kernel.Prelude
import Kernel.Types.Id
import SharedLogic.SharedCab.Allocation.Types (Blame (..))
import SharedLogic.SharedCab.Misses (Charge (..), chargeFor)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

booking :: Id DFTB.FRFSTicketBooking
booking = Id "booking-1"

trip :: Id DVT.VehicleTrip
trip = Id "trip-1"

tests :: TestTree
tests =
  testGroup
    "SharedCab miss counters (R18 05 §8.4)"
    [ testCase "BlameRider charges the booking's no-show counter, not the trip's" $
        chargeFor BlameRider booking (Just "driver-1") (Just ("driver-1", trip)) @?= Just (ChargeBooking booking),
      testCase "BlameRider needs no session or held driver" $
        chargeFor BlameRider booking Nothing Nothing @?= Just (ChargeBooking booking),
      testCase "BlameDriver charges the trip the held driver is running, not the booking" $
        chargeFor BlameDriver booking (Just "driver-1") (Just ("driver-1", trip)) @?= Just (ChargeTrip trip),
      testCase "BlameNone charges nothing, even with a held driver and a live trip" $
        chargeFor BlameNone booking (Just "driver-1") (Just ("driver-1", trip)) @?= Nothing,
      testCase "BlameDriver with the plate now run by another driver charges nobody (never the new driver)" $
        chargeFor BlameDriver booking (Just "driver-1") (Just ("driver-2", trip)) @?= Nothing,
      testCase "BlameDriver with no live session charges nobody (never silently the rider instead)" $
        chargeFor BlameDriver booking (Just "driver-1") Nothing @?= Nothing,
      testCase "BlameDriver with no recorded held driver charges nobody" $
        chargeFor BlameDriver booking Nothing (Just ("driver-1", trip)) @?= Nothing
    ]
