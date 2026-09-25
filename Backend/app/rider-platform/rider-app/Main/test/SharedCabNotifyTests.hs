{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabNotifyTests (tests) where

import "aeson" Data.Aeson (Value (String), toJSON)
import "rider-app" SharedLogic.SharedCab.Notify
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "SharedCab rider notifications"
    [ testCase "keys match the merchant_push_notification rows" $
        map notificationKey [minBound .. maxBound]
          @?= [ "SHARED_CAB_ASSIGNED",
                "SHARED_CAB_ARRIVING",
                "SHARED_CAB_REASSIGNED",
                "SHARED_CAB_BOARD_ANY",
                "SHARED_CAB_ROUTE_CHANGE",
                "SHARED_CAB_DROP_CONFIRM"
              ],
      testCase "the app sees the same string as the key" $
        map toJSON [minBound .. maxBound :: SharedCabNotificationType]
          @?= map (String . notificationKey) [minBound .. maxBound],
      testCase "stop names, with the plate when there is one" $
        templateParams (Just "Malki", "MLK") (Just "Laitumkhrah", "LTK") (Just "ML05A1234")
          @?= [("boardStop", "Malki"), ("dropStop", "Laitumkhrah"), ("vehicleNumber", "ML05A1234")],
      testCase "a stop code stands in for a missing name; no plate, no param" $
        templateParams (Nothing, "MLK") (Just "Laitumkhrah", "LTK") Nothing
          @?= [("boardStop", "MLK"), ("dropStop", "Laitumkhrah")]
    ]
