module Domain.Action.ProviderPlatform.Fleet.RegistrationV2
  ( postRegistrationV2LoginOtp,
    postRegistrationV2VerifyOtp,
    postRegistrationV2Register,
    postRegistrationV2Register',
    RegisterClientCall,
    buildFleetOwnerRegisterReqV2,
  )
where

import qualified API.Client.ProviderPlatform.Fleet as Client
import qualified API.Types.ProviderPlatform.Fleet.RegistrationV2 as Common
import Control.Applicative ((<|>))
import "lib-dashboard" Domain.Action.Dashboard.Registration as DDR
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Person as DP
import qualified Domain.Types.Role as DRole
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.External.Encryption (decrypt, encrypt)
import Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Storage.Queries.Merchant as QMerchant
import "lib-dashboard" Storage.Queries.Person as QP
import qualified Storage.Queries.Role as QRole
import Tools.Auth.Api
import Tools.Auth.Merchant
import "lib-dashboard" Tools.Error

postRegistrationV2LoginOtp ::
  ShortId DM.Merchant ->
  City.City ->
  Common.FleetOwnerLoginReqV2 ->
  Flow APISuccess
postRegistrationV2LoginOtp merchantShortId opCity req = do
  runRequestValidation Common.validateInitiateLoginReqV2 req
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  let enabled = fromMaybe False merchant.verifyFleetWhileLogin
      checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  unless (opCity `elem` merchant.supportedOperatingCities) $ throwError (InvalidRequest "Invalid request city is not supported by Merchant")
  merchantServerAccessCheck merchant
  mbPerson <- QP.findByMobileNumber req.mobileNumber req.mobileCountryCode
  let req' = buildFleetOwnerRegisterReqV2 merchantShortId opCity req
  fleetOwnerRole <- QRole.findByDashboardAccessType DRole.FLEET_OWNER >>= fromMaybeM (RoleNotFound $ show DRole.FLEET_OWNER)
  res <- Client.callFleetAPI checkedMerchantId opCity (.registrationV2DSL.postRegistrationV2LoginOtp) enabled req
  when (isNothing mbPerson) $ do
    let personId = cast @Common.Person @DP.Person res.personId
    createFleetOwnerDashboardOnly fleetOwnerRole merchant req' personId
  pure Success

buildFleetOwnerRegisterReqV2 :: ShortId DM.Merchant -> City.City -> Common.FleetOwnerLoginReqV2 -> FleetRegisterReq
buildFleetOwnerRegisterReqV2 merchantShortId opCity Common.FleetOwnerLoginReqV2 {..} = do
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
  token <- DDR.generateToken person.id merchant opCity
  when (person.verified /= Just True && (merchant.verifyFleetWhileLogin == Just True) && not (fromMaybe False merchant.requireAdminApprovalForFleetOnboarding)) $ QP.updatePersonVerifiedStatus person.id True
  pure $ Common.FleetOwnerVerifyResV2 {authToken = token}

type RegisterClientCall = CheckedShortId DM.Merchant -> City.City -> Text -> Common.FleetOwnerRegisterReqV2 -> Flow Common.FleetOwnerRegisterResV2

postRegistrationV2RegisterClientCall :: RegisterClientCall
postRegistrationV2RegisterClientCall checkedMerchantId opCity requestorId req' =
  Client.callFleetAPI checkedMerchantId opCity (.registrationV2DSL.postRegistrationV2Register) requestorId req'

postRegistrationV2Register ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.FleetOwnerRegisterReqV2 ->
  Flow APISuccess
postRegistrationV2Register = postRegistrationV2Register' postRegistrationV2RegisterClientCall

postRegistrationV2Register' ::
  RegisterClientCall ->
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.FleetOwnerRegisterReqV2 ->
  Flow APISuccess
postRegistrationV2Register' clientCall merchantShortId opCity apiTokenInfo req = do
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  let validateFn = if fromMaybe True merchant.isStrongNameCheckRequired then Common.validateRegisterReqV2 else Common.validateRegisterReqWithLooseCheck
  runRequestValidation validateFn req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  unless (opCity `elem` merchant.supportedOperatingCities) $ throwError (InvalidRequest "Invalid request city is not supported by Merchant")
  let req' = req{Common.adminApprovalRequired = merchant.requireAdminApprovalForFleetOnboarding}
  let requestorId = apiTokenInfo.personId.getId

  fleetOwner <-
    if DP.isFleetOwner apiTokenInfo.person
      then do
        when (isJust req.personId && req.personId /= Just (Id requestorId)) $ throwError AccessDenied
        pure apiTokenInfo.person
      else do
        personId <- req.personId & fromMaybeM (InvalidRequest "personId required")
        person <- QP.findById (cast personId) >>= fromMaybeM (PersonDoesNotExist personId.getId)
        unless (DP.isFleetOwner person) $
          throwError (InvalidRequest "Person should be fleet owner")
        pure person

  whenJust req.email \reqEmail -> do
    fleetOwnerEmail <- forM fleetOwner.email decrypt
    unless (req.email == fleetOwnerEmail) $
      unlessM (isNothing <$> QP.findByEmail reqEmail) $ throwError (InvalidRequest "Email already registered")

  encEmail <- forM req.email encrypt
  let fleetRole = getFleetRole req.fleetType
  fleetOwnerRole <- QRole.findByDashboardAccessType fleetRole >>= fromMaybeM (RoleDoesNotExist $ show fleetRole)

  transaction <- T.buildTransaction (DT.castEndpoint apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing (Just req)
  res <-
    T.withTransactionStoring transaction $
      clientCall checkedMerchantId opCity requestorId req'
  when res.enabled $ do
    unless (fleetOwner.verified == Just True) $ QP.updatePersonVerifiedStatus fleetOwner.id True

  let updFleetOwner = fleetOwner{firstName = req.firstName, lastName = req.lastName, email = encEmail <|> fleetOwner.email}
  QP.updatePerson updFleetOwner.id updFleetOwner
  unless (Just fleetRole == updFleetOwner.dashboardAccessType) $
    QP.updatePersonRole updFleetOwner.id fleetOwnerRole
  pure Success
  where
    getFleetRole mbFleetType = case mbFleetType of
      Just Common.RENTAL_FLEET -> DRole.RENTAL_FLEET_OWNER
      Just Common.NORMAL_FLEET -> DRole.FLEET_OWNER
      Just Common.BUSINESS_FLEET -> DRole.FLEET_OWNER
      Nothing -> DRole.FLEET_OWNER
