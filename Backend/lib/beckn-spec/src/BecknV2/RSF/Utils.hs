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
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney, highPrecMoneyFromText, roundHighPrecMoney)

-- Wire amounts are compared exactly (Σ legs == amount), so they are parsed as exact decimals
-- (never via Double) and normalised to paise.
parseMonetaryString :: Text -> Maybe HighPrecMoney
parseMonetaryString = fmap roundToPaise . highPrecMoneyFromText

roundToPaise :: HighPrecMoney -> HighPrecMoney
roundToPaise = roundHighPrecMoney @HighPrecMoney 2

-- RSF 1.0 (ONDC:NTS10) wire constants (the domain and action are BecknV2.RSF.Enums values). -- Need to discuss which all value we should store merchant wise or city wise
rsfCoreVersion, rsfCountry, rsfCity, rsfTtl :: Text
rsfCoreVersion = "1.0.0"
rsfCountry = "IND"
rsfCity = "*" -- settlement is not city-scoped
rsfTtl = "P2D" -- two days

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
  | RSFNoRecordFound
  | RSFAllocatedUtrMissing
  | RSFInvalidEnum
  | RSFInvalidAmount
  | RSFInvalidCurrency
  deriving (Show, Eq)

rsfErrorCodeToText :: RSFErrorCode -> Text
rsfErrorCodeToText = \case
  RSFMissingMandatory -> "70000"
  RSFInvalidDomain -> "70001"
  RSFInvalidVersion -> "70001"
  RSFInvalidAction -> "70001"
  RSFInvalidSignature -> "70004"
  RSFDuplicateMessage -> "70003"
  RSFNoRecordFound -> "70010"
  RSFAllocatedUtrMissing -> "70011"
  RSFInvalidEnum -> "70001"
  RSFInvalidAmount -> "70020"
  RSFInvalidCurrency -> "70021"

rsfErrorDefaultMessage :: RSFErrorCode -> Text
rsfErrorDefaultMessage = \case
  RSFMissingMandatory -> "Mandatory attributes missing"
  RSFInvalidDomain -> "Invalid domain: expected ONDC:NTS10"
  RSFInvalidVersion -> "Invalid version: expected 1.0.0"
  RSFInvalidAction -> "Invalid action: expected receiver_recon"
  RSFInvalidSignature -> "Invalid signature"
  RSFDuplicateMessage -> "Duplicate message_id"
  RSFNoRecordFound -> "No record found for one or more order ids"
  RSFAllocatedUtrMissing -> "settlement_details omit a UTR already allocated to the order"
  RSFInvalidEnum -> "Wrong enum value or field format"
  RSFInvalidAmount -> "Amount is not a valid number"
  RSFInvalidCurrency -> "Currency is not the merchant's currency"

buildNackForCode :: RSFErrorCode -> Spec.RSFAckResponse
buildNackForCode code = buildNackWithDetail code Nothing

-- The code's message plus what exactly was wrong (e.g. the missing field), for the collector to act on.
buildNackWithDetail :: RSFErrorCode -> Maybe Text -> Spec.RSFAckResponse
buildNackWithDetail code detail =
  Spec.RSFAckResponse
    { rsfAckResponseMessage = Spec.RSFAckMessage {rsfAckMessageAck = Spec.RSFAck {rsfAckStatus = Just "NACK"}},
      rsfAckResponseError = Just $ Spec.RSFError {rsfErrorMessage = Just (rsfErrorDefaultMessage code <> maybe "" (": " <>) detail), rsfErrorCode = Just (rsfErrorCodeToText code)}
    }

buildNack :: Text -> Spec.RSFAckResponse
buildNack errMsg =
  Spec.RSFAckResponse
    { rsfAckResponseMessage = Spec.RSFAckMessage {rsfAckMessageAck = Spec.RSFAck {rsfAckStatus = Just "NACK"}},
      rsfAckResponseError = Just $ Spec.RSFError {rsfErrorMessage = Just errMsg, rsfErrorCode = Nothing}
    }
