 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module PublicTransport.API where

import qualified "public-transport-rider-platform" API.UI.Types as Bap
import Data.Proxy
import qualified "public-transport-rider-platform" Domain.Action.UI.QuoteConfirm as Bap
import qualified "public-transport-rider-platform" Domain.Action.UI.Quotes as Bap
import qualified "public-transport-rider-platform" Domain.Types.Booking.API as Bap
import qualified "public-transport-rider-platform" Domain.Types.Booking.Type as Bap
import qualified "public-transport-rider-platform" Domain.Types.Quote as Bap
import qualified "public-transport-rider-platform" Domain.Types.Search as Bap
import Kernel.Prelude hiding (Proxy)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.App (RegToken)
import Kernel.Types.Id
import Servant hiding (Proxy)
import Servant.Client

userToken :: Text
userToken = "ea37f941-427a-4085-a7d0-96240f166672"

bookingClientM :: RegToken -> Id Bap.Booking -> ClientM Bap.BookingAPIEntity
triggerStatusClientM :: RegToken -> Id Bap.Booking -> ClientM APISuccess
getQuotesClientM :: Id Bap.Search -> RegToken -> ClientM Bap.GetQuotesRes
quoteConfirmClientM :: RegToken -> Id Bap.Quote -> Bap.QConfirmReq -> ClientM Bap.QConfirmRes
(_ :<|> bookingClientM :<|> triggerStatusClientM) :<|> getQuotesClientM :<|> quoteConfirmClientM = client (Proxy @Bap.API)
