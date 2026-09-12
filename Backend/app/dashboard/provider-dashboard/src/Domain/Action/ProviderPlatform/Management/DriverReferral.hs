{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.ProviderPlatform.Management.DriverReferral
  ( postDriverReferralReferralOpsPassword,
    postDriverReferralLinkReferral,
  )
where

import qualified API.Client.ProviderPlatform.Management as Client
import qualified "dynamic-offer-driver-app" API.Types.ProviderPlatform.Management.DriverReferral as Common
import qualified "lib-dashboard" Dashboard.Common as Common
import "dynamic-offer-driver-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow)
import qualified "lib-dashboard" SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import Tools.Auth.Merchant (merchantCityAccessCheck)

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  ApiTokenInfo UserActionType ->
  Maybe request ->
  m (DT.Transaction UserActionType)
buildTransaction apiTokenInfo =
  T.buildTransaction (DT.ActionAPI apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing

postDriverReferralReferralOpsPassword ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo UserActionType ->
  Common.ReferralLinkPasswordUpdateAPIReq ->
  Flow APISuccess
postDriverReferralReferralOpsPassword merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (.driverReferralDSL.postDriverReferralReferralOpsPassword) req

postDriverReferralLinkReferral ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo UserActionType ->
  Common.ReferralLinkReq ->
  Flow Common.LinkReport
postDriverReferralLinkReferral merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callManagementAPI checkedMerchantId opCity (addMultipartBoundary . (.driverReferralDSL.postDriverReferralLinkReferral)) req
  where
    addMultipartBoundary clientFn reqBody = clientFn ("xxxxxxx", reqBody)
