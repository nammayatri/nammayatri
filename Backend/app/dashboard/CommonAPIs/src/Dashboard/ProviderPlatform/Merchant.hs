{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Dashboard.ProviderPlatform.Merchant
  ( module Dashboard.ProviderPlatform.Merchant,
    module Reexport,
  )
where

import Dashboard.Common.Merchant as Reexport
import Kernel.Prelude
import Kernel.Types.Predicate
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import Servant

---------------------------------------------------------
-- merchant update --------------------------------------

type MerchantUpdateAPI =
  "update"
    :> ReqBody '[JSON] MerchantUpdateReq
    :> Post '[JSON] MerchantUpdateRes

data MerchantUpdateReq = MerchantUpdateReq
  { name :: Maybe Text,
    description :: Maybe Text,
    enabled :: Maybe Bool,
    fcmConfig :: Maybe FCMConfigUpdateReq
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MerchantUpdateTReq = MerchantUpdateTReq
  { name :: Maybe Text,
    description :: Maybe Text,
    enabled :: Maybe Bool,
    fcmConfig :: Maybe FCMConfigUpdateTReq
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

validateMerchantUpdateReq :: Validate MerchantUpdateReq
validateMerchantUpdateReq MerchantUpdateReq {..} =
  sequenceA_
    [ validateField "name" name $ InMaybe $ MinLength 3 `And` P.name,
      validateField "description" description $ InMaybe $ MinLength 3 `And` P.name,
      whenJust fcmConfig $ \cfg -> validateObject "fcmConfig" cfg validateFCMConfigUpdateReq
    ]

data MerchantUpdateRes = MerchantUpdateRes
  { name :: Text,
    description :: Maybe Text,
    contactNumber :: Maybe Text,
    status :: Status,
    enabled :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Status = PENDING_VERIFICATION | APPROVED | REJECTED
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets MerchantUpdateReq where
  type ReqWithoutSecrets MerchantUpdateReq = MerchantUpdateTReq
  hideSecrets MerchantUpdateReq {..} =
    MerchantUpdateTReq
      { fcmConfig = hideSecrets <$> fcmConfig,
        ..
      }
