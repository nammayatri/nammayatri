{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.FavouriteDriver 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.FavouriteDriver
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified API.Types.UI.FavouriteDriver
import qualified Data.Text
import qualified Kernel.Types.APISuccess



type API = (TokenAuth :> "driver" :> "favorites" :> Get ('[JSON]) [API.Types.UI.FavouriteDriver.FavouriteDriverResp] :<|> TokenAuth :> "favorites" :> Capture "driverId"
                                                                                                                                                              Data.Text.Text :> "remove" :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)
handler :: Environment.FlowServer API
handler = getDriverFavorites :<|> postFavoritesRemove
getDriverFavorites :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler [API.Types.UI.FavouriteDriver.FavouriteDriverResp])
getDriverFavorites a1 = withFlowHandlerAPI $ Domain.Action.UI.FavouriteDriver.getDriverFavorites (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
postFavoritesRemove :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                         Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Data.Text.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFavoritesRemove a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FavouriteDriver.postFavoritesRemove (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1



