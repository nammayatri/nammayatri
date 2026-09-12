{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Fleet-owner login, OTP verification and registration, served directly.
--
-- provider-dashboard hosted this because it straddles two databases: the fleet
-- owner is a driver-app record, while the dashboard session it ends up holding
-- is an @atlas_dashboard@ one. It reached the driver-app half over HTTP
-- (@ProviderPlatformClient.DynamicOfferDriver.Fleet@).
--
-- Here the driver-app half is a direct call and only the dashboard half needs
-- redirecting, so each dashboard query is wrapped individually in
-- 'runInDashboardDb'. The wrapping is deliberately narrow: widening it to the
-- whole handler would send the fleet-owner reads and writes to the dashboard
-- database, where those tables do not exist.
module API.DashboardFleetRegistration
  ( API,
    handler,
  )
where

import qualified API.Dashboard.Fleet.Registration as DReg
import qualified Domain.Action.Dashboard.Fleet.Registration as DP
import qualified "lib-dashboard" Domain.Action.Dashboard.Registration as DashboardReg
import qualified Domain.Types.FleetOwnerInformation as FOI
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import Environment
import Kernel.Beam.Functions (runInDashboardDb)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified "lib-dashboard" Storage.Beam.SchemaInstances ()
import qualified "lib-dashboard" Storage.Queries.Merchant as QMerchant
import qualified "lib-dashboard" Storage.Queries.Person as QP

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
    :<|> fleetOwnerVerify
    :<|> fleetOwnerRegister

-- | The merchant row is a dashboard row, so this read is the one part that has
-- to change database.
resolveDashboardMerchant :: Text -> Context.City -> Flow DM.Merchant
resolveDashboardMerchant merchantIdText city = do
  let merchantShortId = ShortId merchantIdText :: ShortId DM.Merchant
  merchant <-
    runInDashboardDb $
      QMerchant.findByShortId merchantShortId
        >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  unless (city `elem` merchant.supportedOperatingCities) $
    throwError (InvalidRequest "Invalid request city is not supported by Merchant")
  pure merchant

fleetOwnerLogin :: DP.FleetOwnerLoginReq -> FlowHandler APISuccess
fleetOwnerLogin req = withFlowHandlerAPI' $ do
  void $ resolveDashboardMerchant req.merchantId req.city
  DP.fleetOwnerLogin req

fleetOwnerVerify :: DP.FleetOwnerLoginReq -> FlowHandler DP.FleetOwnerVerifyRes
fleetOwnerVerify req = withFlowHandlerAPI' $ do
  person <-
    runInDashboardDb $
      QP.findByMobileNumber req.mobileNumber req.mobileCountryCode
        >>= fromMaybeM (PersonDoesNotExist req.mobileNumber)
  merchant <- resolveDashboardMerchant req.merchantId req.city
  void $ DP.fleetOwnerVerify req
  token <- runInDashboardDb $ DashboardReg.generateToken person.id merchant req.city
  when (person.verified /= Just True && (merchant.verifyFleetWhileLogin == Just True) && not (fromMaybe False merchant.requireAdminApprovalForFleetOnboarding)) $
    runInDashboardDb $
      QP.updatePersonVerifiedStatus person.id True
  pure $ DP.FleetOwnerVerifyRes {authToken = token}

fleetOwnerRegister :: DP.FleetOwnerRegisterReq -> FlowHandler FleetOwnerRegisterResp
fleetOwnerRegister req = withFlowHandlerAPI' $ do
  merchant <- resolveDashboardMerchant req.merchantId req.city
  let req' = buildFleetOwnerRegisterReq req
      enabled = not $ fromMaybe False merchant.requireAdminApprovalForFleetOnboarding
  res <- DP.fleetOwnerRegister req (Just enabled)
  void $ runInDashboardDb $ DashboardReg.registerFleetOwner req' (Id res.personId)
  token <- runInDashboardDb $ DashboardReg.generateToken (Id res.personId) merchant req.city
  pure $ FleetOwnerRegisterResp "Success" (Just token)

buildFleetOwnerRegisterReq :: DP.FleetOwnerRegisterReq -> DashboardReg.FleetRegisterReq
buildFleetOwnerRegisterReq DP.FleetOwnerRegisterReq {..} =
  DashboardReg.FleetRegisterReq
    { merchantId = ShortId merchantId,
      fleetType = castFleetType fleetType,
      city = Just city,
      ..
    }

castFleetType :: Maybe FOI.FleetType -> Maybe DashboardReg.FleetType
castFleetType = \case
  Just FOI.RENTAL_FLEET -> Just DashboardReg.RENTAL_FLEET
  Just FOI.NORMAL_FLEET -> Just DashboardReg.NORMAL_FLEET
  Just FOI.BUSINESS_FLEET -> Just DashboardReg.BUSINESS_FLEET
  _ -> Nothing
