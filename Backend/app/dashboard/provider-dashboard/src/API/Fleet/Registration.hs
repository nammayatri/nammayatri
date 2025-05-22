{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Fleet.Registration
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Dashboard.Fleet.Registration as Fleet
import Control.Applicative ((<|>))
import qualified "dynamic-offer-driver-app" Domain.Action.Dashboard.Fleet.Registration as DR
import "lib-dashboard" Domain.Action.Dashboard.Registration as DDR
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified "dynamic-offer-driver-app" Domain.Types.FleetOwnerInformation as DFleet
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Person as DP
import qualified Domain.Types.Role as DRole
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.External.Encryption (decrypt, encrypt)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation)
import qualified ProviderPlatformClient.DynamicOfferDriver.Fleet as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Storage.Queries.Merchant as QMerchant
import "lib-dashboard" Storage.Queries.Person as QP
import qualified Storage.Queries.Role as QRole
import Tools.Auth.Api
import "lib-dashboard" Tools.Auth.Merchant
import "lib-dashboard" Tools.Error

type API =
  "fleet"
    :> ( FleetOwnerLoginAPI
           :<|> FleetOwnerVerifyAPI
           :<|> FleetOwnerRegisterAPI
       )

type FleetOwnerLoginAPI =
  "login"
    :> "otp"
    :> ReqBody '[JSON] DR.FleetOwnerLoginReq
    :> Post '[JSON] APISuccess

type FleetOwnerRegisterAPI =
  "register"
    :> ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'FLEET 'DMatrix.REGISTER_FLEET_OWNER
    :> ReqBody '[JSON] DR.FleetOwnerRegisterReq
    :> Post '[JSON] APISuccess

type FleetOwnerVerifyAPI =
  "verify"
    :> "otp"
    :> ReqBody '[JSON] DR.FleetOwnerLoginReq
    :> Post '[JSON] DR.FleetOwnerVerifyRes

handler :: FlowServer API
handler =
  fleetOwnerLogin
    :<|> fleetOwnerVerify
    :<|> fleetOwnerRegister

fleetOwnerLogin :: DR.FleetOwnerLoginReq -> FlowHandler APISuccess
fleetOwnerLogin req = withFlowHandlerAPI' $ do
  runRequestValidation DR.validateInitiateLoginReq req
  let merchantShortId = ShortId req.merchantId :: ShortId DM.Merchant
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  let enabled = not $ fromMaybe False merchant.requireAdminApprovalForFleetOnboarding
      checkedMerchantId = skipMerchantCityAccessCheck merchantShortId

  unless (req.city `elem` merchant.supportedOperatingCities) $ throwError (InvalidRequest "Invalid request city is not supported by Merchant")
  let req' = buildFleetOwnerRegisterReq req
  merchantServerAccessCheck merchant
  mbPerson <- QP.findByMobileNumber req.mobileNumber req.mobileCountryCode
  fleetOwnerRole <- QRole.findByDashboardAccessType DRole.FLEET_OWNER >>= fromMaybeM (RoleNotFound $ show DRole.FLEET_OWNER)
  res <- Client.callDynamicOfferDriverAppFleetApi checkedMerchantId req.city (.registration.fleetOwnerLogin) (Just enabled) req
  case mbPerson of
    Just person -> do
      logError $ "PersonId mismatch for fleet owner registered: dashboard: " <> show person.id <> "; bpp: " <> show res.personId
      pure Success
    Nothing -> do
      let isOperator = False
      void $ createFleetOwnerDashboardOnly fleetOwnerRole merchant req' (Id res.personId) isOperator
      pure Success

fleetOwnerVerify :: DR.FleetOwnerLoginReq -> FlowHandler DR.FleetOwnerVerifyRes
fleetOwnerVerify req = withFlowHandlerAPI' $ do
  person <- QP.findByMobileNumber req.mobileNumber req.mobileCountryCode >>= fromMaybeM (PersonDoesNotExist req.mobileNumber)
  let merchantShortId = ShortId req.merchantId :: ShortId DM.Merchant
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  unless (req.city `elem` merchant.supportedOperatingCities) $ throwError (InvalidRequest "Invalid request city is not supported by Merchant")
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  void $ Client.callDynamicOfferDriverAppFleetApi checkedMerchantId req.city (.registration.fleetOwnerVerify) req
  token <- DDR.generateToken person.id merchant.id req.city
  when (person.verified /= Just True && not (fromMaybe False merchant.requireAdminApprovalForFleetOnboarding)) $ QP.updatePersonVerifiedStatus person.id True
  pure $ DR.FleetOwnerVerifyRes {authToken = token}

fleetOwnerRegister :: ApiTokenInfo -> DR.FleetOwnerRegisterReq -> FlowHandler APISuccess
fleetOwnerRegister apiTokenInfo req = withFlowHandlerAPI' $ do
  runRequestValidation DR.validateRegisterReq req
  let merchantShortId = ShortId req.merchantId :: ShortId DM.Merchant
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  unless (req.city `elem` merchant.supportedOperatingCities) $ throwError (InvalidRequest "Invalid request city is not supported by Merchant")
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId req.city apiTokenInfo.city
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

  transaction <- T.buildTransaction (DT.FleetAPI Fleet.FleetOwnerRegisterEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing (Just req)
  void $
    T.withTransactionStoring transaction $ do
      Client.callDynamicOfferDriverAppFleetApi checkedMerchantId req.city (.registration.fleetOwnerRegister) (Just requestorId) req

  let updFleetOwner = fleetOwner{firstName = req.firstName, lastName = req.lastName, email = encEmail <|> fleetOwner.email}
  QP.updatePerson updFleetOwner.id updFleetOwner
  unless (Just fleetRole == updFleetOwner.dashboardAccessType) $
    QP.updatePersonRole updFleetOwner.id fleetOwnerRole
  pure Success
  where
    getFleetRole mbFleetType = case mbFleetType of
      Just DFleet.RENTAL_FLEET -> DRole.RENTAL_FLEET_OWNER
      Just DFleet.NORMAL_FLEET -> DRole.FLEET_OWNER
      Just DFleet.BUSINESS_FLEET -> DRole.FLEET_OWNER
      Nothing -> DRole.FLEET_OWNER

buildFleetOwnerRegisterReq :: DR.FleetOwnerLoginReq -> FleetRegisterReq
buildFleetOwnerRegisterReq DR.FleetOwnerLoginReq {..} = do
  FleetRegisterReq
    { firstName = "FLEET",
      lastName = "OWNER", -- update in register
      merchantId = ShortId merchantId,
      fleetType = Nothing,
      city = Just city,
      email = Nothing,
      ..
    }
