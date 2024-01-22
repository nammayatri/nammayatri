{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.FRFS where

import qualified API.Beckn.FRFS.OnConfirm as OnConfirm
import qualified API.Beckn.FRFS.OnInit as OnInit
import qualified API.Beckn.FRFS.OnSearch as OnSearch
import qualified API.Beckn.FRFS.OnStatus as OnStatus
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Types.Id
-- import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)

type API =
  "beckn" :> "cab" :> "v1"
    -- :> SignatureAuth "Authorization"
    :> ( OnSearch.API
           :<|> OnInit.API
           :<|> OnConfirm.API
           :<|> OnStatus.API
       )

type APIM =
  "beckn" :> "cab" :> "v1"
    :> Capture "merchantId" (Id DM.Merchant)
    -- :> SignatureAuth "Authorization"
    :> ( OnSearch.API
           :<|> OnInit.API
           :<|> OnConfirm.API
           :<|> OnStatus.API
       )

handler :: FlowServer API
handler =
  OnSearch.handler
    :<|> OnInit.handler
    :<|> OnConfirm.handler
    :<|> OnStatus.handler
