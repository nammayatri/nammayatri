{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.FRFS where

import qualified API.Beckn.FRFS.OnCancel as OnCancel
import qualified API.Beckn.FRFS.OnConfirm as OnConfirm
import qualified API.Beckn.FRFS.OnInit as OnInit
import qualified API.Beckn.FRFS.OnSearch as OnSearch
import qualified API.Beckn.FRFS.OnSelect as OnSelect
import qualified API.Beckn.FRFS.OnStatus as OnStatus
import qualified API.Beckn.FRFS.OnUpdate as OnUpdate
import qualified Domain.Types.Merchant as DM
import Environment
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Id
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)

type API =
  "beckn" :> "frfs" :> "v1"
    :> SignatureAuth 'Domain.PUBLIC_TRANSPORT "Authorization"
    :> ( OnSearch.API
           :<|> OnSelect.API
           :<|> OnInit.API
           :<|> OnConfirm.API
           :<|> OnStatus.API
           :<|> OnCancel.API
           :<|> OnUpdate.API
       )

type APIM =
  "beckn" :> "frfs" :> "v1"
    :> Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth 'Domain.PUBLIC_TRANSPORT "Authorization"
    :> ( OnSearch.API
           :<|> OnSelect.API
           :<|> OnInit.API
           :<|> OnConfirm.API
           :<|> OnStatus.API
           :<|> OnCancel.API
           :<|> OnUpdate.API
       )

handler :: FlowServer API
handler auth =
  OnSearch.handler auth
    :<|> OnSelect.handler auth
    :<|> OnInit.handler auth
    :<|> OnConfirm.handler auth
    :<|> OnStatus.handler auth
    :<|> OnCancel.handler auth
    :<|> OnUpdate.handler auth
