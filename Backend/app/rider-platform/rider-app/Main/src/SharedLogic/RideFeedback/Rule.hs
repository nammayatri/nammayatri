module SharedLogic.RideFeedback.Rule
  ( evaluateRule,
    unsupportedOperators,
    supportedOperatorNames,
    stableBucket,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Data.Char (ord)
import qualified Data.HashSet as HS
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Vector as V
import JsonLogic (jsonLogicEither)
import Kernel.Prelude

-- | Operators of the repo's json-logic-hs fork that were verified to behave as expected.
-- An unknown operator is returned unevaluated by the library and counts as truthy,
-- so rules using anything outside this set are rejected instead of evaluated.
supportedOperators :: HS.HashSet Text
supportedOperators =
  HS.fromList
    [ "var",
      "==",
      "===",
      "!=",
      "!==",
      "<",
      "<=",
      ">",
      ">=",
      "and",
      "or",
      "!",
      "if",
      "?:",
      "in",
      "+",
      "-",
      "*",
      "/",
      "%",
      "max",
      "min",
      "merge",
      "sum",
      "take",
      "drop",
      "coalesce",
      "switch",
      "arrayAt",
      "today",
      "currentTime",
      "dateDiff"
    ]

-- | The supported operators, sorted (shown to dashboard users writing rules).
supportedOperatorNames :: [Text]
supportedOperatorNames = sort (HS.toList supportedOperators)

-- | A missing rule always matches. A rule matches only when it evaluates to exactly @true@.
evaluateRule :: A.ToJSON ctx => Maybe A.Value -> ctx -> Either Text Bool
evaluateRule Nothing _ = Right True
evaluateRule (Just rule) ctx =
  case unsupportedOperators rule of
    ops@(_ : _) -> Left $ "Unsupported json-logic operators: " <> T.intercalate ", " ops
    [] -> case jsonLogicEither rule (A.toJSON ctx) of
      Left err -> Left $ "Rule evaluation failed: " <> show err
      Right (A.Bool True) -> Right True
      Right _ -> Right False

unsupportedOperators :: A.Value -> [Text]
unsupportedOperators = \case
  A.Object obj -> case KM.toList obj of
    [(opKey, args)] ->
      let op = AK.toText opKey
          own = [op | not (HS.member op supportedOperators)]
       in own <> operatorArgs op args
    kvs -> concatMap (unsupportedOperators . snd) kvs
  A.Array items -> concatMap unsupportedOperators (V.toList items)
  _ -> []
  where
    -- The second argument of "switch" is a plain lookup table, not a rule.
    operatorArgs "switch" (A.Array items) = concatMap unsupportedOperators [v | (i, v) <- zip [0 :: Int ..] (V.toList items), i /= 1]
    operatorArgs _ args = unsupportedOperators args

-- | Deterministic bucket in [0, 99]; stable across releases, unlike 'Data.Hashable.hash'.
stableBucket :: Text -> Int
stableBucket = (`mod` 100) . T.foldl' (\acc c -> (acc * 31 + ord c) `mod` 1000000007) 7
