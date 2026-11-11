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
  ( decodeRules,
  )
where

import API.Types.UI.DriverAccountHealth (BehaviourRule (..), RuleCondition (..))
import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import Kernel.Prelude
import Tools.Error (BlockReasonFlag (..))

specMarker :: Text
specMarker = "#rulebook-builder:"

-- | The rules of the table in rulebook order, each copied as written. Nothing when the
-- description has no table, as for a version written by hand, or a table of another format
-- version. The first argument is the engine's map from a block's reasonTag to the flag it sets.
decodeRules :: (Maybe Text -> BlockReasonFlag) -> Text -> Maybe [BehaviourRule]
decodeRules toBlockReasonFlag description = do
  let (upToMarker, spec) = T.breakOnEnd specMarker description
  guard (not $ T.null upToMarker)
  A.decodeStrict (encodeUtf8 spec) >>= A.parseMaybe parseRules
  where
    parseRules = A.withObject "RulebookSpec" $ \o -> do
      formatVersion <- o A..: "v"
      guard (formatVersion == (1 :: Int))
      o A..: "ladder" >>= mapM parseRule
    -- a block keeps its tag in reasonTag; a warning or nudge in overlayKey, which the engine
    -- pins as the warning's reasonTag
    parseRule = A.withObject "Rule" $ \o -> do
      conditions <- o A..: "when" >>= mapM parseCondition
      A.withObject "Action" (parseAction conditions) =<< o A..: "action"
    parseAction conditions a = do
      kind <- a A..: "kind"
      tag <- (<|>) <$> a A..:? "reasonTag" <*> a A..:? "overlayKey"
      hours <- a A..:? "hours"
      pure BehaviourRule {blockReasonFlag = blockReasonFlagOf toBlockReasonFlag kind tag, ..}
    parseCondition = A.withObject "Condition" $ \o ->
      RuleCondition <$> o A..: "fact" <*> o A..: "op" <*> o A..: "value"

-- | The flag the engine sets on the account for this rule. Only a HARD_BLOCK sets one; a tag the
-- engine does not know falls back to ByDashboard there, so it would never point back at this rule.
blockReasonFlagOf :: (Maybe Text -> BlockReasonFlag) -> Text -> Maybe Text -> Maybe BlockReasonFlag
blockReasonFlagOf toBlockReasonFlag kind tag = do
  guard (kind == "HARD_BLOCK")
  reasonTag <- tag
  let flag = toBlockReasonFlag (Just reasonTag)
  guard (flag /= ByDashboard || reasonTag == "ByDashboard")
  pure flag
