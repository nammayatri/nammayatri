{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabPlateTests (tests) where

import "rider-app" SharedLogic.SharedCab.Plate (canonicalisePlate)
import "rider-app" SharedLogic.SharedCab.SpotBooking (CodeResolution (..), isStickerCode, resolveCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "SharedCab plate canonicaliser"
    [ testCase "lowercase with spaces and hyphens" $
        canonicalisePlate "ml 05-a 1234" @?= "ML05A1234",
      testCase "dots and tabs are stripped" $
        canonicalisePlate "ml.05\ta.1234" @?= "ML05A1234",
      testCase "already canonical is unchanged" $
        canonicalisePlate "ML05A1234" @?= "ML05A1234",
      testCase "empty stays empty" $
        canonicalisePlate "" @?= "",
      testGroup
        "walk-up: a typed sticker code resolves among the live cabs"
        [ testCase "exactly one cab ends in the code" $ resolveCode "9999" ["ML05A9999", "ML05B8888"] @?= OneCab "ML05A9999",
          testCase "no cab ends in the code" $ resolveCode "1234" ["ML05A9999", "ML05B8888"] @?= NoCab,
          testCase "two cabs end in the code: the rider picks a route" $ resolveCode "9999" ["ML05A9999", "ML07C9999"] @?= ManyCabs,
          testCase "one cab on two routes is still one cab" $ resolveCode "9999" ["ML05A9999", "ML05A9999"] @?= OneCab "ML05A9999",
          testCase "four digits only" $ map isStickerCode ["9999", "ML05", "99999", "999", ""] @?= [True, False, False, False, False]
        ]
    ]
