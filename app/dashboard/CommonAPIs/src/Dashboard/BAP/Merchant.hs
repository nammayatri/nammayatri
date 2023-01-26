{-# LANGUAGE DerivingStrategies #-}

module Dashboard.BAP.Merchant
  ( module Dashboard.BAP.Merchant,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Predicate
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.Validation
import Dashboard.Common.Merchant as Reexport
import Servant

---------------------------------------------------------
-- merchant update --------------------------------------

type MerchantUpdateAPI =
  "update"
    :> ReqBody '[JSON] MerchantUpdateReq
    :> Post '[JSON] APISuccess

data MerchantUpdateReq = MerchantUpdateReq
  { name :: Maybe Text,
    exoPhone :: Maybe Text,
    exoPhoneCountryCode :: Maybe Text,
    fcmConfig :: Maybe FCMConfigUpdateReq,
    gatewayUrl :: Maybe BaseUrl,
    registryUrl :: Maybe BaseUrl
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MerchantUpdateTReq = MerchantUpdateTReq
  { name :: Maybe Text,
    exoPhone :: Maybe Text,
    exoPhoneCountryCode :: Maybe Text,
    fcmConfig :: Maybe FCMConfigUpdateTReq,
    gatewayUrl :: Maybe BaseUrl,
    registryUrl :: Maybe BaseUrl
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

validateMerchantUpdateReq :: Validate MerchantUpdateReq
validateMerchantUpdateReq MerchantUpdateReq {..} =
  sequenceA_
    [ validateField "name" name $ InMaybe $ MinLength 3 `And` P.name,
      validateField "exoPhone" exoPhone $ InMaybe P.mobileNumber,
      validateField "exoPhoneCountryCode" exoPhoneCountryCode $ InMaybe P.mobileCountryCode,
      validateMbObject "fcmConfig" fcmConfig validateFCMConfigUpdateReq
    ]

instance HideSecrets MerchantUpdateReq where
  type ReqWithoutSecrets MerchantUpdateReq = MerchantUpdateTReq
  hideSecrets MerchantUpdateReq {..} =
    MerchantUpdateTReq
      { fcmConfig = hideSecrets <$> fcmConfig,
        ..
      }
