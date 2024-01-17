{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Dashboard.RideBooking.Profile where

import qualified API.UI.Profile as AP
import qualified Domain.Action.UI.Profile as DProfile
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import SharedLogic.Merchant
import Storage.Beam.SystemConfigs ()

data ProfileEndPoint = UpdatePersonEndPoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord)

derivePersistField "ProfileEndPoint"

type API =
  "profile"
    :> ( CustomerGetProfileAPI
           :<|> CustomerUpdateProfileAPI
       )

type CustomerGetProfileAPI =
  "detail"
    :> Capture "customerId" (Id DP.Person)
    :> Get '[JSON] DProfile.ProfileRes

type CustomerUpdateProfileAPI =
  "update"
    :> Capture "customerId" (Id DP.Person)
    :> ReqBody '[JSON] DProfile.UpdateProfileReq
    :> Post '[JSON] APISuccess

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  callGetPersonDetails merchantId
    :<|> callUpdatePerson merchantId

callGetPersonDetails :: ShortId DM.Merchant -> Id DP.Person -> FlowHandler DProfile.ProfileRes
callGetPersonDetails merchantId personId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantId
  AP.getPersonDetails (personId, m.id)

callUpdatePerson :: ShortId DM.Merchant -> Id DP.Person -> DProfile.UpdateProfileReq -> FlowHandler APISuccess
callUpdatePerson merchantId personId req = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantId
  AP.updatePerson (personId, m.id) req
