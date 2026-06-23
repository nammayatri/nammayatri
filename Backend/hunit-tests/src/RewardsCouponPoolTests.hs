{-# LANGUAGE OverloadedStrings #-}

module RewardsCouponPoolTests (tests) where

import Kernel.Prelude
import Kernel.Types.Id
import Test.Tasty
import Test.Tasty.HUnit
import qualified Tools.Rewards.RedisPool as Pool

tests :: TestTree
tests =
  testGroup
    "Coupon.PoolHelpers"
    [ testCase "poolKey shape" $
        Pool.poolKey (Id "c1") (Id "co1") @?= "reward:pool:c1:co1",
      testCase "inflightKey shape" $
        Pool.inflightKey (Id "c1") (Id "co1") @?= "reward:inflight:c1:co1"
    ]
