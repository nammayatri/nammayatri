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

module API.Dashboard.RideBooking.Maps where

import qualified API.UI.Maps as UM
import qualified Domain.Action.UI.Maps as DMaps
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Error.FlowHandling
import Servant
import SharedLogic.Merchant
import Storage.Beam.SystemConfigs ()

data MapEndPoints
  = AutoCompleteEndPoint
  | GetPlaceDetailsEndPoints
  | GetPlaceNameEndPoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord)

derivePersistField "MapEndPoints"

type API =
  "maps"
    :> ( RideAutoCompleteAPI
           :<|> RideGetPlaceDetailsAPI
           :<|> RideGetPlaceNameAPI
       )

type RideAutoCompleteAPI =
  "autoComplete"
    :> Capture "customerId" (Id DP.Person)
    :> ReqBody '[JSON] DMaps.AutoCompleteReq
    :> Post '[JSON] DMaps.AutoCompleteResp

type RideGetPlaceDetailsAPI =
  "getPlaceDetails"
    :> Capture "customerId" (Id DP.Person)
    :> ReqBody '[JSON] DMaps.GetPlaceDetailsReq
    :> Post '[JSON] DMaps.GetPlaceDetailsResp

type RideGetPlaceNameAPI =
  "getPlaceName"
    :> Capture "customerId" (Id DP.Person)
    :> ReqBody '[JSON] DMaps.GetPlaceNameReq
    :> Post '[JSON] DMaps.GetPlaceNameResp

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  callAutoComplete merchantId
    :<|> callGetPlaceDetails merchantId
    :<|> callGetPlaceName merchantId

callAutoComplete :: ShortId DM.Merchant -> Id DP.Person -> DMaps.AutoCompleteReq -> FlowHandler DMaps.AutoCompleteResp
callAutoComplete merchantId personId req = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantId
  UM.autoComplete (personId, m.id) req

callGetPlaceDetails :: ShortId DM.Merchant -> Id DP.Person -> DMaps.GetPlaceDetailsReq -> FlowHandler DMaps.GetPlaceDetailsResp
callGetPlaceDetails merchantId personId req = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantId
  UM.getPlaceDetails (personId, m.id) req

callGetPlaceName :: ShortId DM.Merchant -> Id DP.Person -> DMaps.GetPlaceNameReq -> FlowHandler DMaps.GetPlaceNameResp
callGetPlaceName merchantId personId req = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantId
  UM.getPlaceName (personId, m.id) req
