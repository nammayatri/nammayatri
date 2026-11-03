{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Management.Driver
  ( module Dashboard.ProviderPlatform.Management.Driver,
    module Reexport,
  )
where

import API.Types.ProviderPlatform.Management.Endpoints.Driver as Reexport
import qualified API.Types.UI.StclMembership as MembershipAPI
import Dashboard.Common as Reexport hiding (Role (..))
import Dashboard.Common.Driver as Reexport hiding (Role (..))
import qualified Data.Csv as Csv
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Kernel.Prelude
import Kernel.ServantMultipart
import Kernel.Types.Predicate
import Kernel.Utils.Common (maskText)
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation

instance HideSecrets [ReviewRCVariantReq] where
  hideSecrets = identity

-- Bulk airport block using csv --

instance FromMultipart Tmp BulkAirportBlockReq where
  fromMultipart form = do
    fileData <- lookupFile "file" form
    let specialZoneId = lookupInput "specialZoneId" form
    pure $
      BulkAirportBlockReq
        { file = fdPayload fileData,
          specialZoneId = case specialZoneId of
            Left _ -> Nothing
            Right x -> Just x
        }

instance ToMultipart Tmp BulkAirportBlockReq where
  toMultipart form = do
    let zoneInput = case form.specialZoneId of
          Nothing -> []
          Just zoneId -> [Input "specialZoneId" zoneId]
    MultipartData zoneInput [FileData "file" (T.pack form.file) "" (form.file)]

data BulkAirportBlockCsvRow = BulkAirportBlockCsvRow
  { driverId :: Text,
    enableForAirport :: AirportRestrictionType,
    blockReason :: Maybe Text,
    blockedDays :: Maybe Int
  }

instance Csv.FromField AirportRestrictionType where
  parseField field =
    case T.toUpper . T.strip . DT.decodeUtf8 $ field of
      "ENABLED" -> pure ENABLED
      "DISABLED" -> pure DISABLED
      "BLOCKED" -> pure BLOCKED
      other -> fail $ "Unsupported enableForAirport: " <> T.unpack other

instance Csv.FromNamedRecord BulkAirportBlockCsvRow where
  parseNamedRecord r =
    BulkAirportBlockCsvRow
      <$> r Csv..: "driverId"
      <*> r Csv..: "enableForAirport"
      <*> r Csv..: "blockReason"
      <*> r Csv..: "blockedDays"

validateUpdatePhoneNumberReq :: Validate UpdatePhoneNumberReq
validateUpdatePhoneNumberReq UpdatePhoneNumberReq {..} =
  sequenceA_
    [ validateField "newPhoneNumber" newPhoneNumber P.mobileNumber,
      validateField "newCountryCode" newCountryCode P.mobileCountryCode
    ]

validateUpdateDriverNameReq :: Validate UpdateDriverNameReq
validateUpdateDriverNameReq UpdateDriverNameReq {..} =
  sequenceA_
    [ validateField "firstName" firstName $ MinLength 1 `And` MaxLength 50 `And` P.name,
      validateField "middleName" middleName $ InMaybe (MaxLength 50 `And` P.middleName),
      validateField "lastName" lastName $ InMaybe (MaxLength 50 `And` P.name)
    ]

validateUpdateDriverNameReqWithLooseCheck :: Validate UpdateDriverNameReq
validateUpdateDriverNameReqWithLooseCheck UpdateDriverNameReq {..} =
  sequenceA_
    [ validateField "firstName" firstName $ MinLength 1 `And` MaxLength 50 `And` P.nameWithSymbols,
      validateField "middleName" middleName $ InMaybe (MaxLength 50 `And` P.nameWithSymbols),
      validateField "lastName" lastName $ InMaybe (MaxLength 50 `And` P.nameWithSymbols)
    ]

instance HideSecrets ClearDriverFeeReq where
  hideSecrets = identity

instance HideSecrets [DriverEncDataReq] where
  hideSecrets = identity

instance HideSecrets MembershipFeeCreateReq where
  hideSecrets req =
    req{aadharNumber = maskText req.aadharNumber,
        panNumber = maskText req.panNumber,
        mobileNumber = maskText req.mobileNumber,
        emailId = maskText req.emailId,
        bankDetails = (\MembershipAPI.BankDetails {..} -> MembershipAPI.BankDetails {accountNumber = maskText accountNumber, ifscCode = maskText ifscCode, ..}) <$> req.bankDetails,
        nomineeInfo = (\MembershipAPI.NomineeInfo {..} -> MembershipAPI.NomineeInfo {nomineeAadhar = maskText <$> nomineeAadhar, ..}) <$> req.nomineeInfo
       }
