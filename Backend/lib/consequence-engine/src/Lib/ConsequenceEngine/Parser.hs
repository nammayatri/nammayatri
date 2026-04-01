{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.ConsequenceEngine.Parser
  ( parseDirective,
    parseDirectives,
  )
where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Kernel.Prelude
import Lib.ConsequenceEngine.Types

-- | Parse a single ConsequenceDirective (from rule-orchestrator) into a typed ConsequenceAction.
--
-- The consequenceType field determines which constructor to use.
-- The params field is parsed into the corresponding param type.
parseDirective :: ConsequenceDirective -> Either Text ConsequenceAction
parseDirective directive =
  case T.toUpper directive.consequenceType of
    "NO_ACTION" -> Right NoAction
    "NUDGE" -> parseParams directive.params Nudge
    "WARN" -> parseParams directive.params Warn
    "SOFT_BLOCK" -> parseParams directive.params SoftBlock
    "FEATURE_BLOCK" -> parseParams directive.params FeatureBlock
    "HARD_BLOCK" -> parseParams directive.params HardBlock
    "PERMANENT_BLOCK" -> parseParams directive.params PermanentBlock
    "CHARGE_FEE" -> parseParams directive.params ChargeFee
    "INCREMENT_COUNTER" -> parseParams directive.params IncrementCounter
    unknown -> Left $ "Unknown consequence type: " <> unknown

-- | Parse all directives, collecting successes and errors
parseDirectives :: [ConsequenceDirective] -> ([ConsequenceAction], [Text])
parseDirectives directives =
  let results = map parseDirective directives
      actions = [a | Right a <- results]
      errors = [e | Left e <- results]
   in (actions, errors)

-- Internal helper: parse JSON params into a FromJSON type and wrap with constructor
parseParams :: (FromJSON a) => A.Value -> (a -> ConsequenceAction) -> Either Text ConsequenceAction
parseParams params constructor =
  case A.fromJSON params of
    A.Success a -> Right (constructor a)
    A.Error err -> Left $ "Failed to parse consequence params: " <> T.pack err
