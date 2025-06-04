module Domain.Action.ProviderPlatform.Fleet.RegistrationV2
  ( postRegistrationV2LoginOtp,
    postRegistrationV2VerifyOtp,
    postRegistrationV2Register,
  )
where

import qualified API.Client.ProviderPlatform.Fleet as Client
import qualified API.Types.ProviderPlatform.Fleet.RegistrationV2 as Common
import "lib-dashboard" Domain.Action.Dashboard.Registration as DR
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Storage.Queries.Merchant as QMerchant
import "lib-dashboard" Storage.Queries.Person as QP
import Tools.Auth.Api
import Tools.Auth.Merchant

postRegistrationV2LoginOtp ::
  ShortId DM.Merchant ->
  City.City ->
  Common.FleetOwnerLoginReqV2 ->
  Flow APISuccess
postRegistrationV2LoginOtp merchantShortId opCity req = do
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  let enabled = not $ fromMaybe False merchant.requireAdminApprovalForFleetOnboarding
      checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  mbPerson <- QP.findByMobileNumber req.mobileNumber req.mobileCountryCode
  unless (opCity `elem` merchant.supportedOperatingCities) $ throwError (InvalidRequest "Invalid request city is not supported by Merchant")
  res <- Client.callFleetAPI checkedMerchantId opCity (.registrationV2DSL.postRegistrationV2LoginOtp) enabled req
  case mbPerson of
    Just _ -> pure Success
    Nothing -> do
      let req' = buildFleetOwnerRegisterReq merchantShortId opCity req
      void $ registerFleetOwner req' $ Just res.personId.getId
      pure Success

buildFleetOwnerRegisterReq :: ShortId DM.Merchant -> City.City -> Common.FleetOwnerLoginReqV2 -> FleetRegisterReq
buildFleetOwnerRegisterReq merchantShortId opCity Common.FleetOwnerLoginReqV2 {..} = do
  FleetRegisterReq
    { firstName = "FLEET",
      lastName = "OWNER", -- update in register
      merchantId = merchantShortId,
      fleetType = Nothing,
      city = Just opCity,
      email = Nothing,
      ..
    }

postRegistrationV2VerifyOtp ::
  ShortId DM.Merchant ->
  City.City ->
  Common.FleetOwnerVerifyReqV2 ->
  Flow Common.FleetOwnerVerifyResV2
postRegistrationV2VerifyOtp merchantShortId opCity req = do
  person <- QP.findByMobileNumber req.mobileNumber req.mobileCountryCode >>= fromMaybeM (PersonDoesNotExist req.mobileNumber)
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  unless (opCity `elem` merchant.supportedOperatingCities) $ throwError (InvalidRequest "Invalid request city is not supported by Merchant")
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  void $ Client.callFleetAPI checkedMerchantId opCity (.registrationV2DSL.postRegistrationV2VerifyOtp) req
  token <- DR.generateToken person.id merchant.id opCity
  when (person.verified /= Just True && not (fromMaybe False merchant.requireAdminApprovalForFleetOnboarding)) $ QP.updatePersonVerifiedStatus person.id True
  pure $ Common.FleetOwnerVerifyResV2 {authToken = token}

postRegistrationV2Register ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.FleetOwnerRegisterReqV2 ->
  Flow APISuccess
postRegistrationV2Register merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  unless (opCity `elem` merchant.supportedOperatingCities) $ throwError (InvalidRequest "Invalid request city is not supported by Merchant")
  let req' = req{Common.adminApprovalRequired = merchant.requireAdminApprovalForFleetOnboarding}
  let requestorId = apiTokenInfo.personId.getId
  transaction <- T.buildTransaction (DT.castEndpoint apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing (Just req)
  res <-
    T.withTransactionStoring transaction $
      Client.callFleetAPI checkedMerchantId opCity (.registrationV2DSL.postRegistrationV2Register) requestorId req'
  when res.enabled $ do
    person <- QP.findById (Id req.personId.getId) >>= fromMaybeM (PersonDoesNotExist req.personId.getId)
    unless (person.verified == Just True) $ QP.updatePersonVerifiedStatus (Id req.personId.getId) True
  pure Success
