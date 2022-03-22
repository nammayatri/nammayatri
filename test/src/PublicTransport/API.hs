module PublicTransport.API where

import qualified "public-transport-bap" API.UI.Types as Bap
import Beckn.Prelude hiding (Proxy)
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.App (RegToken)
import Beckn.Types.Id
import Data.Proxy
import qualified "public-transport-bap" Domain.Action.UI.QuoteConfirm as Bap
import qualified "public-transport-bap" Domain.Action.UI.Quotes as Bap
import qualified "public-transport-bap" Domain.Types.Booking.API as Bap
import qualified "public-transport-bap" Domain.Types.Booking.Type as Bap
import qualified "public-transport-bap" Domain.Types.Quote as Bap
import qualified "public-transport-bap" Domain.Types.Search as Bap
import Servant hiding (Proxy)
import Servant.Client

userToken :: Text
userToken = "ea37f941-427a-4085-a7d0-96240f166672"

bookingClientM :: RegToken -> Id Bap.Booking -> ClientM Bap.BookingAPIEntity
triggerStatusClientM :: RegToken -> Id Bap.Booking -> ClientM APISuccess
getQuotesClientM :: Id Bap.Search -> RegToken -> ClientM Bap.GetQuotesRes
quoteConfirmClientM :: RegToken -> Id Bap.Quote -> Bap.QConfirmReq -> ClientM Bap.QConfirmRes
(_ :<|> bookingClientM :<|> triggerStatusClientM) :<|> getQuotesClientM :<|> quoteConfirmClientM = client (Proxy @Bap.API)
