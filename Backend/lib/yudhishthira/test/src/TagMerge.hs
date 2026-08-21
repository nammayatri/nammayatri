module TagMerge (tagMergeTests) where

import qualified Data.Aeson as A
import Kernel.Prelude
import Lib.Yudhishthira.Tools.Utils (convertTags, upsertTagNameValue)
import qualified Lib.Yudhishthira.Types as LYT
import Test.Tasty
import Test.Tasty.HUnit

tnv :: Text -> LYT.TagNameValueExpiry
tnv = LYT.TagNameValueExpiry

tagMergeTests :: TestTree
tagMergeTests =
  testGroup
    "Multi-value tag merge tests"
    [ convertTagsTests,
      upsertTagNameValueTests
    ]

convertTagsTests :: TestTree
convertTagsTests =
  testGroup
    "convertTags"
    [ testCase "single value per name stays scalar (no behaviour change)" $
        convertTags [tnv "Cohort#MahilaShakti"]
          @?= A.object ["Cohort" A..= ("MahilaShakti" :: Text)],
      testCase "two values under one name merge into an array" $
        convertTags [tnv "Cohort#A", tnv "Cohort#B"]
          @?= A.object ["Cohort" A..= (["A", "B"] :: [Text])],
      testCase "an unrelated name is unaffected by a multi-value name" $
        convertTags [tnv "Cohort#A", tnv "Cohort#B", tnv "DriverTier#Gold"]
          @?= A.object
            [ "Cohort" A..= (["A", "B"] :: [Text]),
              "DriverTier" A..= ("Gold" :: Text)
            ]
    ]

upsertTagNameValueTests :: TestTree
upsertTagNameValueTests =
  testGroup
    "upsertTagNameValue"
    [ testCase "allowMultiple=False replaces the whole name" $
        upsertTagNameValue False (Just [tnv "Cohort#A"]) (tnv "Cohort#B")
          @?= [tnv "Cohort#B"],
      testCase "allowMultiple=True keeps siblings and replaces the exact pair in place" $
        upsertTagNameValue True (Just [tnv "Cohort#A", tnv "Cohort#B"]) (tnv "Cohort#B#2026-01-01T00:00:00")
          @?= [tnv "Cohort#A", tnv "Cohort#B#2026-01-01T00:00:00"],
      testCase "allowMultiple=True appends a new pair" $
        upsertTagNameValue True (Just [tnv "Cohort#A"]) (tnv "Cohort#B")
          @?= [tnv "Cohort#A", tnv "Cohort#B"],
      testCase "allowMultiple=True on an empty list yields the single pair" $
        upsertTagNameValue True Nothing (tnv "Cohort#A")
          @?= [tnv "Cohort#A"]
    ]
