{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Operator.Registration (module ReExport, validateOperatorRegisterReq) where

import API.Types.ProviderPlatform.Operator.Endpoints.Registration
import Dashboard.Common as ReExport
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Predicate as P
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation

instance HideSecrets OperatorRegisterReq where
  type ReqWithoutSecrets OperatorRegisterReq = OperatorRegisterTReq
  hideSecrets OperatorRegisterReq {..} = OperatorRegisterTReq {..}

validateOperatorRegisterReq :: Context.Country -> Validate OperatorRegisterReq
validateOperatorRegisterReq country OperatorRegisterReq {..} =
  sequenceA_
    [ validateField "firstName" firstName $ P.MinLength 1 `P.And` P.MaxLength 50 `P.And` P.name,
      validateField "lastName" lastName $ P.MaxLength 50 `P.And` P.name,
      validateField "mobileNumber" mobileNumber $ P.getMobileNumberPredicate country,
      validateField "mobileCountryCode" mobileCountryCode $ P.getMobileCountryCodePredicate country,
      validateField "email" email $ P.InMaybe P.email
    ]
