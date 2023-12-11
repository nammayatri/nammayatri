{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn (API, APIV2, handler) where

import qualified API.Beckn.OnCancel as OnCancel
import qualified API.Beckn.OnConfirm as OnConfirm
import qualified API.Beckn.OnInit as OnInit
import qualified API.Beckn.OnSearch as OnSearch
import qualified API.Beckn.OnSelect as OnSelect
import qualified API.Beckn.OnStatus as OnStatus
import qualified API.Beckn.OnTrack as OnTrack
import qualified API.Beckn.OnUpdate as OnUpdate
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Types.Id
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)

type API =
  "cab" :> "v1" :> SignatureAuth "Authorization"
    :> ( OnSearch.API
           :<|> OnSelect.API
           :<|> OnInit.API
           :<|> OnConfirm.API
           :<|> OnUpdate.API
           :<|> OnStatus.API
           :<|> OnTrack.API
           :<|> OnCancel.API
       )

type APIV2 =
  "beckn" :> "cab" :> "v1"
    :> Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth "Authorization"
    :> ( OnSearch.API
           :<|> OnSelect.API
           :<|> OnInit.API
           :<|> OnConfirm.API
           :<|> OnUpdate.API
           :<|> OnStatus.API
           :<|> OnTrack.API
           :<|> OnCancel.API
       )

handler :: FlowServer API
handler auth =
  OnSearch.handler auth
    :<|> OnSelect.handler auth
    :<|> OnInit.handler auth
    :<|> OnConfirm.handler auth
    :<|> OnUpdate.handler auth
    :<|> OnStatus.handler auth
    :<|> OnTrack.handler auth
    :<|> OnCancel.handler auth
