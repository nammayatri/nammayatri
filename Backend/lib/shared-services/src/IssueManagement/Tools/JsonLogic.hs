-- | Minimal json-logic evaluator for ApiCall branch conditions. Rules are the
-- same shape the Config Pilot dashboard's LogicBuilder emits, evaluated over
-- the chat flow context (extracted response fields + builtins).
--
-- Supported operators: var, ==, !=, >, >=, <, <=, in, and/&&, or/||, !.
-- Anything unsupported evaluates to False - a branch must never match by
-- accident, and the flow's default/onError paths catch the rest.
module IssueManagement.Tools.JsonLogic (evalBool) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Vector as V
import EulerHS.Prelude

evalBool :: A.Value -> Map.Map Text A.Value -> Bool
evalBool rule ctx = truthy (eval rule ctx)

eval :: A.Value -> Map.Map Text A.Value -> A.Value
eval v ctx = case v of
  A.Object obj -> case AKM.toList obj of
    [(op, args)] -> applyOp (AK.toText op) args ctx
    _ -> A.Bool False
  other -> other

applyOp :: Text -> A.Value -> Map.Map Text A.Value -> A.Value
applyOp op args ctx = case op of
  "var" -> case eval args ctx of
    A.String name -> fromMaybe A.Null (lookupVar name ctx)
    _ -> A.Null
  "==" -> binary (\a b -> A.Bool (looseEq a b))
  "!=" -> binary (\a b -> A.Bool (not (looseEq a b)))
  ">" -> numCmp (>)
  ">=" -> numCmp (>=)
  "<" -> numCmp (<)
  "<=" -> numCmp (<=)
  "in" -> binary memberOf
  "and" -> conj
  "&&" -> conj
  "or" -> disj
  "||" -> disj
  "!" -> case argList of
    [x] -> A.Bool (not (truthy (eval x ctx)))
    _ -> A.Bool False
  _ -> A.Bool False
  where
    argList = case args of
      A.Array arr -> V.toList arr
      single -> [single]

    binary f = case argList of
      [a, b] -> f (eval a ctx) (eval b ctx)
      _ -> A.Bool False

    numCmp f = binary $ \a b -> case (asNumber a, asNumber b) of
      (Just x, Just y) -> A.Bool (f x y)
      _ -> A.Bool False

    conj = A.Bool (all (truthy . (`eval` ctx)) argList)
    disj = A.Bool (any (truthy . (`eval` ctx)) argList)

-- | Dot-path lookup into the context, so conditions can reach into extracted
-- objects: {"var": "ride.status"}.
lookupVar :: Text -> Map.Map Text A.Value -> Maybe A.Value
lookupVar name ctx = case T.splitOn "." name of
  [] -> Nothing
  (root : rest) -> Map.lookup root ctx >>= descend rest
  where
    descend [] v = Just v
    descend (seg : segs) (A.Object obj) = AKM.lookup (AK.fromText seg) obj >>= descend segs
    descend (seg : segs) (A.Array arr) = readMaybe (T.unpack seg) >>= (arr V.!?) >>= descend segs
    descend _ _ = Nothing

-- | Comparison that tolerates the string/number mismatch inherent in template
-- systems: extracted "5" equals configured 5.
looseEq :: A.Value -> A.Value -> Bool
looseEq a b
  | a == b = True
  | otherwise = case (asNumber a, asNumber b) of
    (Just x, Just y) -> x == y
    _ -> False

asNumber :: A.Value -> Maybe Scientific
asNumber (A.Number n) = Just n
asNumber (A.String s) = readMaybe (T.unpack s)
asNumber _ = Nothing

memberOf :: A.Value -> A.Value -> A.Value
memberOf needle (A.Array hay) = A.Bool (any (looseEq needle) (V.toList hay))
memberOf (A.String needle) (A.String hay) = A.Bool (needle `T.isInfixOf` hay)
memberOf _ _ = A.Bool False

truthy :: A.Value -> Bool
truthy (A.Bool b) = b
truthy A.Null = False
truthy (A.Number n) = n /= 0
truthy (A.String s) = not (T.null s)
truthy (A.Array a) = not (V.null a)
truthy (A.Object _) = True
