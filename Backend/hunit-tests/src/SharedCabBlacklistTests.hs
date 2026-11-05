{-# LANGUAGE PackageImports #-}

module SharedCabBlacklistTests (tests) where

import "beckn-spec" BecknV2.FRFS.Enums (ServiceTierType (..))
import "rider-app" Lib.JourneyModule.Utils (getBlacklistedFilters)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Prelude

tests :: TestTree
tests =
  testGroup
    "SharedCab multimodal service-tier blacklist"
    [ testCase "SHARED_CAB survives the filter for every flag / list combination" $
        mapM_
          ( \(flag, newTiers) ->
              assertBool (show (flag, newTiers)) $
                SHARED_CAB `notElem` fst (getBlacklistedFilters flag newTiers)
          )
          [(flag, newTiers) | flag <- [Nothing, Just True, Just False], newTiers <- [Nothing, Just [], Just [PREMIUM], Just [SHUTTLE]]],
      testCase "default filter still drops SHUTTLE (filter is live)" $
        assertBool "SHUTTLE blacklisted by default" $
          SHUTTLE `elem` fst (getBlacklistedFilters Nothing Nothing)
    ]
