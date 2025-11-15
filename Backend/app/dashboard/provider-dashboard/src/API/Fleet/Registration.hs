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

import qualified "dynamic-offer-driver-app" API.Dashboard.Fleet.Registration as DReg
import qualified "dynamic-offer-driver-app" Domain.Action.Dashboard.Fleet.Registration as DP
import qualified Domain.Action.Dashboard.Registration as DashboardReg
import "lib-dashboard" Domain.Action.Dashboard.Registration as DR
import qualified "dynamic-offer-driver-app" Domain.Types.FleetOwnerInformation as FOI
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
    :> ( DReg.FleetOwnerLoginAPI
           :<|> FleetOwnerVerifyAPI
           :<|> FleetOwnerRegisterAPI
       )

type FleetOwnerRegisterAPI =
  "register"
    :> ReqBody '[JSON] DP.FleetOwnerRegisterReq
    :> Post '[JSON] FleetOwnerRegisterResp

type FleetOwnerVerifyAPI =
  "verify"
    :> "otp"
    :> ReqBody '[JSON] DP.FleetOwnerLoginReq
    :> Post '[JSON] DP.FleetOwnerVerifyRes

data FleetOwnerRegisterResp = FleetOwnerRegisterResp
  { result :: Text,
    authToken :: Maybe Text
  }
  deriving (Show, ToJSON, FromJSON, Generic, ToSchema)

handler :: FlowServer API
handler =
  fleetOwnerLogin
    :<|> fleetOwnerVerfiy
    :<|> fleetOwnerRegister

fleetOwnerLogin :: DP.FleetOwnerLoginReq -> FlowHandler APISuccess
fleetOwnerLogin req = withFlowHandlerAPI' $ do
  let merchantShortId = ShortId req.merchantId :: ShortId DM.Merchant
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  unless (req.city `elem` merchant.supportedOperatingCities) $ throwError (InvalidRequest "Invalid request city is not supported by Merchant")
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  Client.callDynamicOfferDriverAppFleetApi checkedMerchantId req.city (.registration.fleetOwnerLogin) req

fleetOwnerVerfiy :: DP.FleetOwnerLoginReq -> FlowHandler DP.FleetOwnerVerifyRes
fleetOwnerVerfiy req = withFlowHandlerAPI' $ do
  person <- QP.findByMobileNumber req.mobileNumber req.mobileCountryCode >>= fromMaybeM (PersonDoesNotExist req.mobileNumber)
  let merchantShortId = ShortId req.merchantId :: ShortId DM.Merchant
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  unless (req.city `elem` merchant.supportedOperatingCities) $ throwError (InvalidRequest "Invalid request city is not supported by Merchant")
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  void $ Client.callDynamicOfferDriverAppFleetApi checkedMerchantId req.city (.registration.fleetOwnerVerify) req
  token <- DR.generateToken person.id merchant req.city
  when (person.verified /= Just True && (merchant.verifyFleetWhileLogin == Just True) && not (fromMaybe False merchant.requireAdminApprovalForFleetOnboarding)) $ QP.updatePersonVerifiedStatus person.id True
  pure $ DP.FleetOwnerVerifyRes {authToken = token}

fleetOwnerRegister :: DP.FleetOwnerRegisterReq -> FlowHandler FleetOwnerRegisterResp
fleetOwnerRegister req = withFlowHandlerAPI' $ do
  let merchantShortId = ShortId req.merchantId :: ShortId DM.Merchant
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  unless (req.city `elem` merchant.supportedOperatingCities) $ throwError (InvalidRequest "Invalid request city is not supported by Merchant")
  let req' = buildFleetOwnerRegisterReq req
      checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
      enabled = not $ fromMaybe False merchant.requireAdminApprovalForFleetOnboarding
  res <- Client.callDynamicOfferDriverAppFleetApi checkedMerchantId req.city (.registration.fleetOwnerRegister) (Just enabled) req
  void $ registerFleetOwner req' (Id res.personId)
  token <- DR.generateToken (Id res.personId) merchant req.city
  pure $ FleetOwnerRegisterResp "Success" (Just token)

buildFleetOwnerRegisterReq :: DP.FleetOwnerRegisterReq -> FleetRegisterReq
buildFleetOwnerRegisterReq DP.FleetOwnerRegisterReq {..} = do
  FleetRegisterReq
    { merchantId = ShortId merchantId,
      fleetType = castFleetType fleetType,
      city = Just city,
      ..
    }

castFleetType :: Maybe FOI.FleetType -> Maybe DashboardReg.FleetType
castFleetType req = case req of
  Just FOI.RENTAL_FLEET -> Just DashboardReg.RENTAL_FLEET
  Just FOI.NORMAL_FLEET -> Just DashboardReg.NORMAL_FLEET
  Just FOI.BUSINESS_FLEET -> Just DashboardReg.BUSINESS_FLEET
  _ -> Nothing
