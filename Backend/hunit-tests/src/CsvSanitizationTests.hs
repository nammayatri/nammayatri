{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | Tests for the CSV formula-injection encoding in Tools.Csv.
--
-- The fare-policy CSV is a round-trip format: exported, edited, re-uploaded. That makes
-- 'sanitizeCsvField' and 'desanitizeCsvField' a matched pair, and a mismatch between them is
-- silent — a value fails 'readMaybe' and is dropped, or is rewritten to a different value, on the
-- endpoint that sets pricing. Both failure modes have shipped here before, so the round-trip
-- property is pinned rather than left implied.
module CsvSanitizationTests where

import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import "dynamic-offer-driver-app" Tools.Csv
  ( desanitizeCsvField,
    hasCsvFormulaPrefix,
    needsCsvQuoting,
    sanitizeCsvField,
  )
import Prelude

-- | Every value the other tests reason about, in one place so the round-trip property and the
-- neutralization property are checked over exactly the same corpus.
corpus :: [T.Text]
corpus =
  [ -- plain values, must pass through untouched
    "",
    "24x7",
    "normal text",
    "Bangalore",
    "a=b",
    "Save > 20%",
    -- formula triggers
    "=cmd()",
    "+1",
    "-5",
    "@SUM(A1)",
    "=1+1",
    -- leading whitespace: a spreadsheet ignores it, so the check must too
    "\t=cmd()",
    " -5",
    "\r\n=cmd()",
    -- apostrophe-prefixed values the user actually typed
    "'24x7' service",
    "'Best' plan",
    "'",
    "''",
    "'=cmd()",
    "'-5",
    "''=cmd()",
    -- apostrophe that is not in leading position
    " '=x",
    "don't"
  ]

-- | The property that matters: import must undo export exactly, for every input.
--
-- An earlier implementation quoted only formula-prefixed values, which made the encoding
-- non-injective ("=x" and "'=x" both became "'=x") and silently rewrote "'-5" to "-5".
testRoundTrip :: TestTree
testRoundTrip =
  testGroup
    "desanitizeCsvField . sanitizeCsvField == id"
    [ testCase (show value) $ desanitizeCsvField (sanitizeCsvField value) @?= value
      | value <- corpus
    ]

-- | Export must leave nothing a spreadsheet would evaluate. This is the actual security property;
-- the round-trip test alone would be satisfied by doing nothing at all.
testNeutralization :: TestTree
testNeutralization =
  testGroup
    "exported values are inert"
    [ testCase (show value) $
        assertBool "formula-like value was not quoted on export" $
          not (hasCsvFormulaPrefix (sanitizeCsvField value))
      | value <- corpus
    ]

-- | Regressions worth naming, each of which was a real defect at some point.
testKnownRegressions :: TestTree
testKnownRegressions =
  testGroup
    "known regressions"
    [ testCase "negative numbers survive the round trip (readMaybe would reject \"'-5\")" $
        desanitizeCsvField (sanitizeCsvField "-5") @?= "-5",
      testCase "apostrophe-prefixed prose is not quoted and not truncated" $ do
        sanitizeCsvField "'24x7' service" @?= "'24x7' service"
        desanitizeCsvField "'24x7' service" @?= "'24x7' service",
      testCase "a user-typed \"'-5\" is not rewritten to \"-5\"" $
        desanitizeCsvField (sanitizeCsvField "'-5") @?= "'-5",
      testCase "a user-typed \"'=cmd()\" is not turned back into a live formula" $
        desanitizeCsvField (sanitizeCsvField "'=cmd()") @?= "'=cmd()",
      testCase "only ambiguous values pay the extra apostrophe" $ do
        sanitizeCsvField "'=cmd()" @?= "''=cmd()"
        sanitizeCsvField "'Best' plan" @?= "'Best' plan",
      testCase "a spreadsheet that ate the apostrophe still imports cleanly" $
        -- Excel/Sheets drop the text-format marker when re-saving, so the exported "'-5" can
        -- arrive back as "-5". With nothing to strip, import is the identity.
        desanitizeCsvField "-5" @?= "-5"
    ]

testFormulaPrefixDetection :: TestTree
testFormulaPrefixDetection =
  testGroup
    "hasCsvFormulaPrefix"
    [ testCase "detects each trigger character" $
        map hasCsvFormulaPrefix ["=x", "+x", "-x", "@x"] @?= [True, True, True, True],
      testCase "sees through leading whitespace, as a spreadsheet does" $
        map hasCsvFormulaPrefix ["\t=x", " =x", "\r\n=x"] @?= [True, True, True],
      testCase "does not fire on a trigger in non-leading position" $
        map hasCsvFormulaPrefix ["a=b", "Save > 20%", "don't", ""] @?= [False, False, False, False],
      testCase "an apostrophe-quoted formula is already inert" $
        hasCsvFormulaPrefix "'=x" @?= False,
      testCase "needsCsvQuoting counts through stacked apostrophes" $
        map needsCsvQuoting ["=x", "'=x", "''=x", "'Best' plan", "24x7"]
          @?= [True, True, True, False, False]
    ]

csvSanitizationTests :: TestTree
csvSanitizationTests =
  testGroup
    "CSV Formula Injection Sanitization"
    [ testRoundTrip,
      testNeutralization,
      testKnownRegressions,
      testFormulaPrefixDetection
    ]
