{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Reads the rule table the control-center rulebook builder saves in a version's
-- description, after "#rulebook-builder:" (control-center chitragupta/utils/compileRulebook.ts).
-- The engine runs the compiled logic; this table is the same rules in readable form.
module SharedLogic.BehaviourManagement.RulebookSpec
  ( Rung,
    decodeLadder,
    warnLimit,
    blockLimit,
  )
where

import API.Types.UI.DriverAccountHealth (RuleLimit (..))
import Control.Applicative (optional)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Text as T
import Kernel.Prelude

data Condition = Condition
  { fact :: Text,
    op :: Text,
    value :: Maybe Double
  }

data Rung = Rung
  { conditions :: [Condition],
    actionKind :: Text,
    actionHours :: Maybe Int
  }

specMarker :: Text
specMarker = "#rulebook-builder:"

-- | The ladder, top rung first. Nothing when the description has no table, as for a
-- version written by hand, or a table of another format version.
decodeLadder :: Text -> Maybe [Rung]
decodeLadder description = do
  let (upToMarker, spec) = T.breakOnEnd specMarker description
  guard (not $ T.null upToMarker)
  A.decodeStrict (encodeUtf8 spec) >>= A.parseMaybe parseLadder
  where
    parseLadder = A.withObject "RulebookSpec" $ \o -> do
      formatVersion <- o A..: "v"
      guard (formatVersion == (1 :: Int))
      o A..: "ladder" >>= mapM parseRung
    parseRung = A.withObject "Rung" $ \o -> do
      conditions <- o A..: "when" >>= mapM parseCondition
      (actionKind, actionHours) <- o A..: "action" >>= A.withObject "Action" (\a -> (,) <$> a A..: "kind" <*> optional (a A..: "hours"))
      pure Rung {..}
    -- a condition's value can also be a boolean or a string; those are never limits
    parseCondition = A.withObject "Condition" $ \o ->
      Condition <$> o A..: "fact" <*> o A..: "op" <*> optional (o A..: "value")

-- | The period's WARN rung. A rulebook has one rung of each kind per period.
warnLimit :: Text -> [Rung] -> Maybe RuleLimit
warnLimit = limitOf (== "WARN")

-- | The period's HARD_BLOCK or PERMANENT_BLOCK rung.
blockLimit :: Text -> [Rung] -> Maybe RuleLimit
blockLimit = limitOf (`elem` ["HARD_BLOCK", "PERMANENT_BLOCK"])

-- | The rung of this kind that sets a lower bound on this period's counters.
limitOf :: (Text -> Bool) -> Text -> [Rung] -> Maybe RuleLimit
limitOf isKind period ladder = listToMaybe $ mapMaybe rungLimit (filter (isKind . (.actionKind)) ladder)
  where
    rungLimit rung = do
      -- every condition must hold, so several bounds on one counter give the largest
      let atLeast counter = fmap maximum . nonEmpty $ mapMaybe lowestPassing [c | c <- rung.conditions, c.fact == "counters." <> period <> "." <> counter]
          limit = RuleLimit {rate = atLeast "rate", minRides = atLeast "eligibleCount", minCancellations = atLeast "actionCount", hours = rung.actionHours}
      guard (isJust limit.rate || isJust limit.minRides || isJust limit.minCancellations)
      pure limit

-- | The counters are whole numbers, so a lower bound is the smallest whole number that
-- passes it. Any other condition, an exact match included, is no limit.
lowestPassing :: Condition -> Maybe Int
lowestPassing condition = do
  bound <- condition.value
  case condition.op of
    ">=" -> Just (ceiling bound)
    ">" -> Just (floor bound + 1)
    _ -> Nothing
