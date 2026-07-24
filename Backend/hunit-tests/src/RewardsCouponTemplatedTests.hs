{-# LANGUAGE OverloadedStrings #-}

module RewardsCouponTemplatedTests (tests) where

import qualified Domain.Action.Rewards.Coupon as Coupon
import Kernel.Prelude
import Kernel.Types.Id
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Coupon.renderTemplate"
    [ testCase "substitutes personId" $
        Coupon.renderTemplate "NY-{personId}" (Id "abc-123") (Id "camp-1")
          @?= "NY-abc-123",
      testCase "substitutes campaignId" $
        Coupon.renderTemplate "X-{campaignId}-Y" (Id "p1") (Id "diwali")
          @?= "X-diwali-Y",
      testCase "substitutes both" $
        Coupon.renderTemplate "{campaignId}/{personId}" (Id "p1") (Id "c1")
          @?= "c1/p1",
      testCase "no placeholders is identity" $
        Coupon.renderTemplate "STATIC-CODE" (Id "p1") (Id "c1")
          @?= "STATIC-CODE"
    ]
