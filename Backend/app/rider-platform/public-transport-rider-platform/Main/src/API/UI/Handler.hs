{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Handler where

import qualified API.UI.Booking.Handler as Booking
import qualified API.UI.QuoteConfirm.Handler as QuoteConfirm
import qualified API.UI.SearchId.Quotes.Handler as Quotes
import qualified API.UI.Types as UI
import Environment
import Servant

handler :: FlowServer UI.API
handler =
  Booking.handler
    :<|> Quotes.handler
    :<|> QuoteConfirm.handler
