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
