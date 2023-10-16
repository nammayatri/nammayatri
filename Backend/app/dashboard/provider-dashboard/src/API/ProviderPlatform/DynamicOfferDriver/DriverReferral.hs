{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.DriverReferral
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.DriverReferral as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, withFlowHandlerAPI)
import qualified ProviderPlatformClient.DynamicOfferDriver as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant (merchantCityAccessCheck)

type API =
  "referralProgram"
    :> ( ReferralProgramPasswordUpdateAPI
           :<|> ReferralProgramLinkCodeAPI
       )

type ReferralProgramPasswordUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'REFERRAL 'REFERRAL_PROGRAM_PASSWORD_UPDATE
    :> Common.ReferralProgramPasswordUpdateAPI

type ReferralProgramLinkCodeAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'REFERRAL 'REFERRAL_PROGRAM_LINK_CODE
    :> Common.ReferralProgramLinkCodeAPI

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  updateReferralLinkPassword merchantId city
    :<|> linkDriverReferralCode merchantId city

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.ReferralEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.DriverReferralAPI endpoint) (Just DRIVER_OFFER_BPP) (Just apiTokenInfo) Nothing Nothing

updateReferralLinkPassword ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.ReferralLinkPasswordUpdateAPIReq ->
  FlowHandler APISuccess
updateReferralLinkPassword merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.ReferralProgramUpdateOpsPasswordEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.driverReferral.updateReferralLinkPassword) req

linkDriverReferralCode ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.ReferralLinkReq ->
  FlowHandler Common.LinkReport
linkDriverReferralCode merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.ReferralProgramUpdateOpsPasswordEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (addMultipartBoundary . (.driverReferral.linkDriverReferralCode)) req
  where
    addMultipartBoundary clientFn reqBody = clientFn ("xxxxxxx", reqBody)
