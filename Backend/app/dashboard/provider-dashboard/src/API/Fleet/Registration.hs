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

import qualified "dynamic-offer-driver-app" Domain.Action.Dashboard.Fleet.Registration as DP
import "lib-dashboard" Domain.Action.Dashboard.Registration as DR
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified ProviderPlatformClient.DynamicOfferDriver.Fleet as Client
import Servant hiding (throwError)
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Storage.Queries.Merchant as QMerchant
import "lib-dashboard" Storage.Queries.Person as QP
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "fleet"
    :> ( FleetOwnerLoginAPI
           :<|> FleetOwnerVerifyAPI
           :<|> FleetOwnerRegisterAPI
       )

type FleetOwnerLoginAPI =
  "login"
    :> "otp"
    :> ReqBody '[JSON] DP.FleetOwnerLoginReq
    :> Post '[JSON] APISuccess

type FleetOwnerRegisterAPI =
  "register"
    :> ReqBody '[JSON] DP.FleetOwnerRegisterReq
    :> Post '[JSON] APISuccess

type FleetOwnerVerifyAPI =
  "verify"
    :> "otp"
    :> ReqBody '[JSON] DP.FleetOwnerLoginReq
    :> Post '[JSON] DP.FleetOwnerVerifyRes

handler :: FlowServer API
handler =
  fleetOwnerLogin
    :<|> fleetOwnerVerfiy
    :<|> fleetOwnerRegister

fleetOwnerLogin :: DP.FleetOwnerLoginReq -> FlowHandler APISuccess
fleetOwnerLogin req = withFlowHandlerAPI' $ do
  let merchantShortId = ShortId req.merchantId :: ShortId DM.Merchant
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  let enabled = not $ fromMaybe False merchant.requireAdminApprovalForFleetOnboarding
      checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  mbPerson <- QP.findByMobileNumber req.mobileNumber req.mobileCountryCode
  unless (req.city `elem` merchant.supportedOperatingCities) $ throwError (InvalidRequest "Invalid request city is not supported by Merchant")
  res <- Client.callDynamicOfferDriverAppFleetApi checkedMerchantId req.city (.registration.fleetOwnerLogin) (Just enabled) req
  case mbPerson of
    Just _ -> pure Success
    Nothing -> do
      let req' = buildFleetOwnerRegisterReq req
      void $ registerFleetOwner False req' $ Just res.personId
      pure Success

fleetOwnerVerfiy :: DP.FleetOwnerLoginReq -> FlowHandler DP.FleetOwnerVerifyRes
fleetOwnerVerfiy req = withFlowHandlerAPI' $ do
  person <- QP.findByMobileNumber req.mobileNumber req.mobileCountryCode >>= fromMaybeM (PersonDoesNotExist req.mobileNumber)
  let merchantShortId = ShortId req.merchantId :: ShortId DM.Merchant
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  unless (req.city `elem` merchant.supportedOperatingCities) $ throwError (InvalidRequest "Invalid request city is not supported by Merchant")
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  void $ Client.callDynamicOfferDriverAppFleetApi checkedMerchantId req.city (.registration.fleetOwnerVerify) req
  token <- DR.generateToken person.id merchant.id req.city
  when (person.verified /= Just True && not (fromMaybe False merchant.requireAdminApprovalForFleetOnboarding)) $ QP.updatePersonVerifiedStatus person.id True
  pure $ DP.FleetOwnerVerifyRes {authToken = token}

fleetOwnerRegister :: DP.FleetOwnerRegisterReq -> FlowHandler APISuccess
fleetOwnerRegister req = withFlowHandlerAPI' $ do
  let merchantShortId = ShortId req.merchantId :: ShortId DM.Merchant
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  unless (req.city `elem` merchant.supportedOperatingCities) $ throwError (InvalidRequest "Invalid request city is not supported by Merchant")
  let checkedMerchantId = skipMerchantCityAccessCheck merchant.shortId
      req' = req {DP.adminApprovalRequired = merchant.requireAdminApprovalForFleetOnboarding}
  res <- Client.callDynamicOfferDriverAppFleetApi checkedMerchantId req.city (.registration.fleetOwnerRegister) req' -- transactions
  when res.enabled $ do
    person <- QP.findById (Kernel.Types.Id.Id req.personId.getId) >>= fromMaybeM (PersonDoesNotExist req.personId.getId)
    unless (person.verified == Just True) $ QP.updatePersonVerifiedStatus (Kernel.Types.Id.Id req.personId.getId) True
  pure Success

buildFleetOwnerRegisterReq :: DP.FleetOwnerLoginReq -> FleetRegisterReq
buildFleetOwnerRegisterReq DP.FleetOwnerLoginReq {..} = do
  FleetRegisterReq
    { firstName = "FLEET",
      lastName = "OWNER", -- update in register
      merchantId = ShortId merchantId,
      fleetType = Nothing,
      city = Just city,
      email = Nothing,
      ..
    }
