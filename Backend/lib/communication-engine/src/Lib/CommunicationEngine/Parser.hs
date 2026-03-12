{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.CommunicationEngine.Parser
  ( parseDirective,
    parseDirectives,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Kernel.Prelude
import Lib.CommunicationEngine.Types
import Lib.RuleOrchestrator.Types (CommunicationDirective (..))

-- | Parse a single CommunicationDirective (from rule-orchestrator) into a typed CommunicationAction.
--
-- The channel field determines which constructor to use.
-- The params field is parsed into the corresponding param type.
-- The templateKey is extracted and merged into the params.
parseDirective :: CommunicationDirective -> Either Text CommunicationAction
parseDirective directive =
  case T.toUpper directive.channel of
    "FCM_NOTIFICATION" ->
      Right $
        FcmNotification
          FcmNotificationParams
            { templateKey = directive.templateKey,
              templateParams = directive.params
            }
    "NOTIFICATION" ->
      -- Alias for FCM_NOTIFICATION (matches design doc naming)
      Right $
        FcmNotification
          FcmNotificationParams
            { templateKey = directive.templateKey,
              templateParams = directive.params
            }
    "IN_APP_OVERLAY" ->
      Right $
        InAppOverlay
          InAppOverlayParams
            { overlayKey = directive.templateKey,
              templateParams = directive.params,
              showCloseButton = extractBoolParam "showCloseButton" directive.params True
            }
    "OVERLAY" ->
      -- Alias for IN_APP_OVERLAY (matches design doc naming)
      Right $
        InAppOverlay
          InAppOverlayParams
            { overlayKey = directive.templateKey,
              templateParams = directive.params,
              showCloseButton = extractBoolParam "showCloseButton" directive.params True
            }
    "IN_APP_MESSAGE" ->
      Right $
        InAppMessage
          InAppMessageParams
            { messageKey = directive.templateKey,
              templateParams = directive.params,
              expiryHours = extractIntParam "expiryHours" directive.params
            }
    "SMS" ->
      Right $
        SmsCommunication
          SmsParams
            { templateKey = directive.templateKey,
              templateParams = directive.params
            }
    "BADGE" ->
      Right $
        BadgeCommunication
          BadgeParams
            { badgeKey = directive.templateKey,
              badgeType = extractTextParam "badgeType" directive.params "WARNING",
              expiryHours = extractIntParam "expiryHours" directive.params
            }
    "NO_COMMUNICATION" -> Right NoCommunication
    unknown -> Left $ "Unknown communication channel: " <> unknown

-- | Parse all directives, collecting successes and errors
parseDirectives :: [CommunicationDirective] -> ([CommunicationAction], [Text])
parseDirectives directives =
  let results = map parseDirective directives
      actions = [a | Right a <- results]
      errors = [e | Left e <- results]
   in (actions, errors)

-- Internal helpers for extracting optional params from JSON

extractBoolParam :: Text -> A.Value -> Bool -> Bool
extractBoolParam key (A.Object obj) defaultVal =
  case KM.lookup (AK.fromText key) obj of
    Just val -> case A.fromJSON val of
      A.Success v -> v
      _ -> defaultVal
    Nothing -> defaultVal
extractBoolParam _ _ defaultVal = defaultVal

extractIntParam :: Text -> A.Value -> Maybe Int
extractIntParam key (A.Object obj) =
  case KM.lookup (AK.fromText key) obj of
    Just val -> case A.fromJSON val of
      A.Success v -> Just v
      _ -> Nothing
    Nothing -> Nothing
extractIntParam _ _ = Nothing

extractTextParam :: Text -> A.Value -> Text -> Text
extractTextParam key (A.Object obj) defaultVal =
  case KM.lookup (AK.fromText key) obj of
    Just val -> case A.fromJSON val of
      A.Success v -> v
      _ -> defaultVal
    Nothing -> defaultVal
extractTextParam _ _ defaultVal = defaultVal
