{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.IGM where

import qualified API.Beckn.IGM.OnIssue as OnIssue
import qualified API.Beckn.IGM.OnIssueStatus as OnIssueStatus
import qualified Domain.Types.Merchant as DM
import Environment
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Id
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)

type API =
  "beckn" :> "igm" :> "v1"
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> ( OnIssue.API
           :<|> OnIssueStatus.API
       )

type APIM =
  "beckn" :> "igm" :> "v1"
    :> Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> ( OnIssue.API
           :<|> OnIssueStatus.API
       )

handler :: FlowServer API
handler auth =
  OnIssue.handler auth
    :<|> OnIssueStatus.handler auth
