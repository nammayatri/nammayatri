module API.Internal.FavouriteDrivers where

import qualified Domain.Action.Internal.FavouriteDrivers as Domain
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

type API =
  Capture "merchantId" (Id Merchant)
    :> "getFavouriteDrivers"
    :> Header "token" Text
    :> ReqBody '[JSON] Domain.GetFavouriteDriverInfoReq
    :> Get '[JSON] [Domain.FavouriteDriverResp]
    :<|> Capture "merchantId" (Id Merchant)
      :> Capture "driverId" (Id Person.Person)
      :> "removeFavouriteDriver"
      :> Header "token" Text
      :> ReqBody '[JSON] Domain.GetFavouriteDriverInfoReq
      :> Post '[JSON] APISuccess

handler :: FlowServer API
handler = getFavouriteDrivers :<|> removeFavouriteDriver

getFavouriteDrivers :: Id Merchant -> Maybe Text -> Domain.GetFavouriteDriverInfoReq -> FlowHandler [Domain.FavouriteDriverResp]
getFavouriteDrivers merchantId apiKey = withFlowHandlerAPI . Domain.getFavouriteDrivers merchantId apiKey

removeFavouriteDriver :: Id Merchant -> Id Person.Person -> Maybe Text -> Domain.GetFavouriteDriverInfoReq -> FlowHandler APISuccess
removeFavouriteDriver merchantId driverId apiKey = withFlowHandlerAPI . Domain.removeFavouriteDriver merchantId driverId apiKey
