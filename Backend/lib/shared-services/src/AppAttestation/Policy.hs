{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AppAttestation.Policy
  ( PolicyActions (..),
    decide,
    enforcedFor,
    enforces,
    actionToDecision,
  )
where

import AppAttestation.Domain.Types.Common
import AppAttestation.Types
import Kernel.Prelude

data PolicyActions = PolicyActions
  { onInvalid :: AttestationAction,
    onMissing :: AttestationAction,
    onUnsupportedOrError :: AttestationAction
  }
  deriving (Show, Eq)

actionToDecision :: AttestationAction -> Decision
actionToDecision = \case
  ALLOW -> DecisionAllow
  FALLBACK -> DecisionFallback
  SILENT_DROP -> DecisionSilentDrop
  REJECT -> DecisionReject

decide :: AttestationMode -> PolicyActions -> UserKind -> Outcome -> Decision
decide mode PolicyActions {..} userKind outcome
  | not (enforcedFor mode userKind) = DecisionAllow
  | otherwise = case outcome of
    VALID -> DecisionAllow
    INVALID -> actionToDecision onInvalid
    MISSING -> actionToDecision onMissing
    UNSUPPORTED -> actionToDecision (if onUnsupportedOrError == ALLOW then FALLBACK else onUnsupportedOrError)
    ERROR -> actionToDecision onUnsupportedOrError

enforcedFor :: AttestationMode -> UserKind -> Bool
enforcedFor mode userKind = case mode of
  OFF -> False
  SHADOW -> False
  ENFORCE_NEW_USERS -> userKind == NewUser
  ENFORCE_ALL -> True

enforces :: AttestationMode -> Bool
enforces mode = any (enforcedFor mode) [minBound .. maxBound]
