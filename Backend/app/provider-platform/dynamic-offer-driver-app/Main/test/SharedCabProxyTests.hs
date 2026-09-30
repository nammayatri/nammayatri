{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | R54: the driver's cancel reason survives every layer of the driver-app proxy on its way to rider-app.
module SharedCabProxyTests (tests) where

import "dynamic-offer-driver-app" API.Types.UI.SharedCab (CancelBookingReq (..))
import Data.Aeson (Value (..), decode, encode, object, (.=))
import "dynamic-offer-driver-app" Domain.Action.UI.SharedCabBooking (BookingAction (..), actionReason)
import "dynamic-offer-driver-app" SharedLogic.CallSharedCabBooking (BAPDriverReq (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

-- what rider-app's SharedCabDriverReq decodes: driverId, vehicleNumber, reason (Maybe)
forwarded :: BAPDriverReq -> Maybe Value
forwarded = decode . encode

tests :: TestTree
tests =
  testGroup
    "shared-cab driver proxy (R54 cancel reason)"
    [ testCase "the app's body {reason} decodes" $ decode "{\"reason\":\"rider not at stop\"}" @?= Just (CancelBookingReq {reason = "rider not at stop"}),
      testCase "a body without a reason is rejected at the door" $ (decode "{}" :: Maybe CancelBookingReq) @?= Nothing,
      testCase "the cancel action carries its reason on" $ actionReason (Cancel "cab breakdown") @?= Just "cab breakdown",
      testCase "other actions carry none" $ map actionReason [BoardedWithoutCode, Dropped] @?= [Nothing, Nothing],
      testCase "the request to rider-app has the reason under \"reason\"" $
        forwarded (BAPDriverReq {driverId = "d1", vehicleNumber = "ML05A9999", reason = Just "cab breakdown"})
          @?= Just (object ["driverId" .= ("d1" :: String), "vehicleNumber" .= ("ML05A9999" :: String), "reason" .= ("cab breakdown" :: String)]),
      testCase "another action sends reason null (rider-app reads it as Nothing)" $
        forwarded (BAPDriverReq {driverId = "d1", vehicleNumber = "ML05A9999", reason = Nothing})
          @?= Just (object ["driverId" .= ("d1" :: String), "vehicleNumber" .= ("ML05A9999" :: String), "reason" .= Null])
    ]
