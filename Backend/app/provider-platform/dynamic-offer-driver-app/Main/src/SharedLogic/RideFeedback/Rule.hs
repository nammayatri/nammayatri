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
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import JsonLogic (isOperatorKey, jsonLogicEither, operationKeys, specialOperationKeys)
import Kernel.Prelude

-- | Whether the repo's json-logic-hs fork evaluates this operator, asked of the library itself
-- (a trailing @'@ is the library's key-eating variant of the same operator). An unknown
-- operator is returned unevaluated by the library and counts as truthy, so rules using
-- anything else are rejected instead of evaluated.
isSupportedOperator :: Text -> Bool
isSupportedOperator op = isOperatorKey (AK.fromText (fromMaybe op (T.stripSuffix "'" op)))

-- | The supported operators, sorted (shown to dashboard users writing rules).
supportedOperatorNames :: [Text]
supportedOperatorNames = map AK.toText (Set.toAscList (specialOperationKeys <> operationKeys))

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
          own = [op | not (isSupportedOperator op)]
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
