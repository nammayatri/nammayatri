{-# LANGUAGE DerivingStrategies #-}

module Dashboard.BPP.Merchant
  ( module Dashboard.BPP.Merchant,
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
      validateMbObject "fcmConfig" fcmConfig validateFCMConfigUpdateReq
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
