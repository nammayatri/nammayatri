{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Management.Driver.Referral where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.DriverReferral as Common
import qualified Domain.Action.Dashboard.DriverReferral as DRe
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (Unauthorized, throwError)

type API =
  "referralProgram"
    :> ( Common.ReferralProgramPasswordUpdateAPI
           :<|> Common.ReferralProgramLinkCodeAPI
       )

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler merchantId city =
  updateReferralLinkPassword merchantId city
    :<|> linkDriverReferralCode merchantId city

updateReferralLinkPassword ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.ReferralLinkPasswordUpdateAPIReq ->
  FlowHandler APISuccess
updateReferralLinkPassword merchantShortId opCity = withFlowHandlerAPI . DRe.updateReferralLinkPassword merchantShortId opCity

linkDriverReferralCode ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.ReferralLinkReq ->
  FlowHandler Common.LinkReport
linkDriverReferralCode merchantShortId opCity = withFlowHandlerAPI . DRe.linkDriverReferralCode merchantShortId opCity
