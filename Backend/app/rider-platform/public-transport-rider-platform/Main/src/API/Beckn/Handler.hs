{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Handler where

import qualified API.Beckn.OnCancel.Handler as OnCancel
import qualified API.Beckn.OnConfirm.Handler as OnConfirm
import qualified API.Beckn.OnSearch.Handler as OnSearch
import qualified API.Beckn.OnStatus.Handler as OnStatus
import qualified API.Beckn.Types as Beckn
import Environment
import Servant

handler :: FlowServer Beckn.API
handler auth =
  OnSearch.handler auth
    :<|> OnConfirm.handler auth
    :<|> OnStatus.handler auth
    :<|> OnCancel.handler auth
