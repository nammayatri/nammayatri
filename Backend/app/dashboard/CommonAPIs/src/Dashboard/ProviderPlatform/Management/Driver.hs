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
import Dashboard.Common as Reexport
import Dashboard.Common.Driver as Reexport
import Kernel.Prelude
import Kernel.Types.Predicate
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation

instance HideSecrets [ReviewRCVariantReq] where
  hideSecrets = identity

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
      validateField "middleName" middleName $ InMaybe (MaxLength 50 `And` P.name),
      validateField "lastName" lastName $ InMaybe (MaxLength 50 `And` P.name)
    ]

validateIndianMobileNumber :: Validate UpdatePhoneNumberReq
validateIndianMobileNumber UpdatePhoneNumberReq {..} =
  sequenceA_
    [ validateField "newPhoneNumber" newPhoneNumber P.indianMobileNumber,
      validateField "newCountryCode" newCountryCode P.mobileIndianCode
    ]

instance HideSecrets ClearDriverFeeReq where
  hideSecrets = identity

instance HideSecrets [DriverEncDataReq] where
  hideSecrets = identity
