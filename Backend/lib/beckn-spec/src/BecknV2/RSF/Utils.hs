{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module BecknV2.RSF.Utils where

import qualified BecknV2.RSF.Types as Spec
import qualified Data.Text as T
import Data.Time (addUTCTime)
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.TimeRFC339 (UTCTimeRFC3339)

parseMonetaryString :: Text -> Maybe HighPrecMoney
parseMonetaryString = readMaybe . T.unpack

parseDeadline :: UTCTime -> Maybe Text -> Maybe UTCTime
parseDeadline now mTtl = do
  ttl <- mTtl
  seconds <- parseISO8601DurationToSeconds ttl
  pure $ addUTCTime (fromIntegral seconds) now

parseISO8601DurationToSeconds :: Text -> Maybe Int
parseISO8601DurationToSeconds t = do
  let s = T.unpack t
  case s of
    'P' : rest
      | null rest -> Nothing
      | otherwise -> parseDuration rest 0 False
    _ -> Nothing
  where
    parseDuration [] acc hadComponent
      | hadComponent = Just acc
      | otherwise = Nothing
    parseDuration ('T' : rest) acc hadComponent = parseTimePart rest acc hadComponent
    parseDuration rest acc _hadComponent = do
      let (numStr, remaining) = span isDigit rest
      num <- readMaybe numStr
      case remaining of
        'D' : rest' -> parseDuration rest' (acc + num * 86400) True
        _ -> Nothing

    parseTimePart [] acc hadComponent
      | hadComponent = Just acc
      | otherwise = Nothing
    parseTimePart rest acc _hadComponent = do
      let (numStr, remaining) = span isDigit rest
      num <- readMaybe numStr
      case remaining of
        'H' : rest' -> parseTimePart rest' (acc + num * 3600) True
        'M' : rest' -> parseTimePart rest' (acc + num * 60) True
        'S' : rest' -> parseTimePart rest' (acc + num) True
        _ -> Nothing

    isDigit c = c >= '0' && c <= '9'

buildRSFContext ::
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  UTCTimeRFC3339 ->
  Maybe Text ->
  Spec.RSFContext
buildRSFContext domain action bapId bapUri bppId bppUri messageId timestamp ttl =
  Spec.RSFContext
    { rsfContextDomain = Just domain,
      rsfContextCountry = Just "IND",
      rsfContextCity = Nothing,
      rsfContextAction = Just action,
      rsfContextCoreVersion = Just "1.0.0",
      rsfContextBapId = Just bapId,
      rsfContextBapUri = Just bapUri,
      rsfContextBppId = Just bppId,
      rsfContextBppUri = Just bppUri,
      rsfContextTransactionId = Nothing,
      rsfContextMessageId = Just messageId,
      rsfContextTimestamp = Just timestamp,
      rsfContextTtl = ttl
    }

buildAck :: Spec.RSFAckResponse
buildAck =
  Spec.RSFAckResponse
    { rsfAckResponseMessage = Spec.RSFAckMessage {rsfAckMessageAck = Spec.RSFAck {rsfAckStatus = Just "ACK"}},
      rsfAckResponseError = Nothing
    }

data RSFErrorCode
  = RSFMissingMandatory
  | RSFInvalidDomain
  | RSFInvalidVersion
  | RSFInvalidAction
  | RSFInvalidSignature
  | RSFDuplicateMessage
  deriving (Show, Eq)

-- Per the ONDC RSF v1.0 error table: 70001 ("Enumeration - Wrong Enum Value
-- Provided") is the single generic bucket for every fixed-value field check
-- -- domain, action, and core_version are all instances of the same
-- category, not separate codes. 70002 is "wrong key/value format" (a
-- different failure mode entirely) and 70003 is "Duplicate Transaction ID"
-- -- neither has anything to do with domain/action/version mismatches.
rsfErrorCodeToText :: RSFErrorCode -> Text
rsfErrorCodeToText = \case
  RSFMissingMandatory -> "70000"
  RSFInvalidDomain -> "70001"
  RSFInvalidVersion -> "70001"
  RSFInvalidAction -> "70001"
  RSFInvalidSignature -> "70004"
  RSFDuplicateMessage -> "70005"

rsfErrorDefaultMessage :: RSFErrorCode -> Text
rsfErrorDefaultMessage = \case
  RSFMissingMandatory -> "Mandatory attributes missing"
  RSFInvalidDomain -> "Invalid domain: expected ONDC:NTS10"
  RSFInvalidVersion -> "Invalid version: expected 1.0.0"
  RSFInvalidAction -> "Invalid action: expected receiver_recon"
  RSFInvalidSignature -> "Invalid signature"
  RSFDuplicateMessage -> "Duplicate message_id"

buildNackForCode :: RSFErrorCode -> Spec.RSFAckResponse
buildNackForCode code =
  Spec.RSFAckResponse
    { rsfAckResponseMessage = Spec.RSFAckMessage {rsfAckMessageAck = Spec.RSFAck {rsfAckStatus = Just "NACK"}},
      rsfAckResponseError = Just $ Spec.RSFError {rsfErrorMessage = Just (rsfErrorDefaultMessage code), rsfErrorCode = Just (rsfErrorCodeToText code)}
    }

buildNack :: Text -> Spec.RSFAckResponse
buildNack errMsg =
  Spec.RSFAckResponse
    { rsfAckResponseMessage = Spec.RSFAckMessage {rsfAckMessageAck = Spec.RSFAck {rsfAckStatus = Just "NACK"}},
      rsfAckResponseError = Just $ Spec.RSFError {rsfErrorMessage = Just errMsg, rsfErrorCode = Nothing}
    }
