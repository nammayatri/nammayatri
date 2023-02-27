{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn (API, handler) where

import qualified API.Beckn.Cancel as Cancel
import qualified API.Beckn.Confirm as Confirm
import qualified API.Beckn.Init as Init
import qualified API.Beckn.Rating as Rating
import qualified API.Beckn.Search as Search
import qualified API.Beckn.Select as Select
import qualified API.Beckn.Track as Track
import Environment
import Servant

type API =
  "beckn"
    :> ( Search.API
           :<|> Select.API
           :<|> Init.API
           :<|> Confirm.API
           :<|> Track.API
           :<|> Cancel.API
           :<|> Rating.API
       )

handler :: FlowServer API
handler =
  Search.handler
    :<|> Select.handler
    :<|> Init.handler
    :<|> Confirm.handler
    :<|> Track.handler
    :<|> Cancel.handler
    :<|> Rating.handler
