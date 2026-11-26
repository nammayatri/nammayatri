{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AppAttestation.Types
  ( Platform (..),
    Outcome (..),
    Reason (..),
    AttestationResult (..),
    Decision (..),
    UserKind (..),
    Endpoint (..),
    AttestationHeaders (..),
    Evidence (..),
    emptyHeaders,
    parseEvidence,
    resultOf,
    withVerdictSummary,
  )
where

import Data.Aeson (Value)
import qualified Data.Text as T
import Kernel.Prelude

data Platform
  = ANDROID_PLAY_INTEGRITY
  | IOS_APP_ATTEST
  deriving (Show, Eq, Ord, Read, Enum, Bounded, Generic, ToJSON, FromJSON, ToSchema)

data Outcome
  = VALID
  | INVALID
  | MISSING
  | UNSUPPORTED
  | ERROR
  deriving (Show, Eq, Ord, Read, Enum, Bounded, Generic, ToJSON, FromJSON, ToSchema)

data Reason
  = NO_ATTESTATION
  | MISSING_ON_ATTESTING_VERSION
  | HEADERS_INCOMPLETE
  | UNKNOWN_PLATFORM
  | CLIENT_REPORTED_UNSUPPORTED
  | CHALLENGE_UNKNOWN
  | CHALLENGE_REPLAYED
  | TOKEN_TOO_LARGE
  | PLATFORM_NOT_CONFIGURED
  | PLATFORM_ERROR
  | PLATFORM_TIMEOUT
  | TOKEN_DECODE_FAILED
  | VERDICT_MALFORMED
  | PACKAGE_MISMATCH
  | REQUEST_HASH_MISMATCH
  | TOKEN_EXPIRED
  | APP_NOT_RECOGNIZED
  | CERT_MISMATCH
  | DEVICE_INTEGRITY_LOW
  | APP_UNLICENSED
  | APP_ACCESS_RISK
  | DEVICE_ACTIVITY_HIGH
  | KEY_NOT_REGISTERED
  | KEY_REVOKED
  | KEY_BUSY
  | KEY_VELOCITY_EXCEEDED
  | APP_NOT_ALLOWED
  | ATTESTATION_MALFORMED
  | CERT_CHAIN_INVALID
  | NONCE_MISMATCH
  | KEY_ID_MISMATCH
  | COUNTER_NOT_ZERO
  | ENVIRONMENT_MISMATCH
  | ASSERTION_MALFORMED
  | RP_ID_MISMATCH
  | SIGNATURE_INVALID
  | COUNTER_NOT_INCREASING
  deriving (Show, Eq, Ord, Read, Enum, Bounded, Generic, ToJSON, FromJSON, ToSchema)

data AttestationResult = AttestationResult
  { platform :: Maybe Platform,
    outcome :: Outcome,
    reasons :: [Reason],
    verdictSummary :: Maybe Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

resultOf :: Maybe Platform -> Outcome -> [Reason] -> AttestationResult
resultOf platform outcome reasons = AttestationResult {verdictSummary = Nothing, ..}

withVerdictSummary :: Value -> AttestationResult -> AttestationResult
withVerdictSummary summary result = result {verdictSummary = Just summary}

data Decision
  = DecisionAllow
  | DecisionFallback
  | DecisionSilentDrop
  | DecisionReject
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data UserKind
  = NewUser
  | ExistingUser
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data Endpoint
  = AuthEndpoint
  | ResendEndpoint
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data AttestationHeaders = AttestationHeaders
  { platform :: Maybe Text,
    challengeId :: Maybe Text,
    token :: Maybe Text,
    keyId :: Maybe Text,
    assertion :: Maybe Text,
    clientError :: Maybe Text,
    captchaToken :: Maybe Text
  }
  deriving (Show, Eq, Generic)

emptyHeaders :: AttestationHeaders
emptyHeaders = AttestationHeaders Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data Evidence
  = AndroidEvidence {challengeId :: Text, token :: Text}
  | IosEvidence {challengeId :: Text, keyId :: Text, assertion :: Text}
  | ClientUnsupported (Maybe Platform) Text
  | NoEvidence
  | MalformedEvidence (Maybe Platform) Reason
  deriving (Show, Eq)

parseEvidence :: AttestationHeaders -> Evidence
parseEvidence AttestationHeaders {..} =
  case (nonEmpty' platform, nonEmpty' clientError) of
    (Nothing, Nothing)
      | all isNothing [nonEmpty' challengeId, nonEmpty' token, nonEmpty' keyId, nonEmpty' assertion] -> NoEvidence
      | otherwise -> MalformedEvidence Nothing HEADERS_INCOMPLETE
    (mbPlatform, Just err) -> ClientUnsupported (mbPlatform >>= readMaybe . T.unpack) err
    (Just rawPlatform, Nothing) -> case readMaybe (T.unpack rawPlatform) of
      Nothing -> MalformedEvidence Nothing UNKNOWN_PLATFORM
      Just ANDROID_PLAY_INTEGRITY ->
        maybe (MalformedEvidence (Just ANDROID_PLAY_INTEGRITY) HEADERS_INCOMPLETE) identity $
          AndroidEvidence <$> nonEmpty' challengeId <*> nonEmpty' token
      Just IOS_APP_ATTEST ->
        maybe (MalformedEvidence (Just IOS_APP_ATTEST) HEADERS_INCOMPLETE) identity $
          IosEvidence <$> nonEmpty' challengeId <*> nonEmpty' keyId <*> nonEmpty' assertion
  where
    nonEmpty' = (>>= \v -> let s = T.strip v in if T.null s then Nothing else Just s)
