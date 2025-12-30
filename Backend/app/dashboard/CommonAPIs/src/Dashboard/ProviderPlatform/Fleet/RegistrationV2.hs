{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Fleet.RegistrationV2 (module ReExport, validateRegisterReqV2, validateRegisterReqWithLooseCheck, validateInitiateLoginReqV2) where

import API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2
import Dashboard.Common as ReExport
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Predicate as P
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation

instance HideSecrets FleetOwnerRegisterReqV2 where
  type ReqWithoutSecrets FleetOwnerRegisterReqV2 = FleetOwnerRegisterTReqV2
  hideSecrets FleetOwnerRegisterReqV2 {..} = FleetOwnerRegisterTReqV2 {..}

validateRegisterReqV2 :: Validate FleetOwnerRegisterReqV2
validateRegisterReqV2 FleetOwnerRegisterReqV2 {..} =
  sequenceA_
    [ validateField "firstName" firstName $ P.NotEmpty `P.And` P.name,
      validateField "lastName" lastName $ P.NotEmpty `P.And` P.name,
      validateField "email" email $ P.InMaybe P.email
    ]

validateRegisterReqWithLooseCheck :: Validate FleetOwnerRegisterReqV2
validateRegisterReqWithLooseCheck FleetOwnerRegisterReqV2 {..} =
  sequenceA_
    [ validateField "firstName" firstName $ P.NotEmpty `P.And` P.nameWithNumber,
      validateField "lastName" lastName P.nameWithNumber,
      validateField "email" email $ P.InMaybe P.email
    ]

validateInitiateLoginReqV2 :: Context.Country -> Validate FleetOwnerLoginReqV2
validateInitiateLoginReqV2 country FleetOwnerLoginReqV2 {..} =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber $ P.getMobileNumberPredicate country,
      validateField "mobileCountryCode" mobileCountryCode $ P.getMobileCountryCodePredicate country
    ]
