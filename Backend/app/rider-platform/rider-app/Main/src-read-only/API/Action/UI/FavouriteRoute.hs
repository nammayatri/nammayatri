{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.FavouriteRoute
  ( API,
    handler,
  )
where

import qualified API.Types.UI.FavouriteRoute
import qualified Control.Lens
import qualified Domain.Action.UI.FavouriteRoute
import qualified Domain.Types.FavouriteRoute
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "favouriteRoute" :> "create" :> ReqBody ('[JSON]) API.Types.UI.FavouriteRoute.CreateFavouriteRouteReq
      :> Post
           ('[JSON])
           API.Types.UI.FavouriteRoute.FavouriteRouteAPIEntity
      :<|> TokenAuth
      :> "favouriteRoute"
      :> "list"
      :> Get
           ('[JSON])
           [API.Types.UI.FavouriteRoute.FavouriteRouteAPIEntity]
      :<|> TokenAuth
      :> "favouriteRoute"
      :> Capture
           "favouriteId"
           (Kernel.Types.Id.Id Domain.Types.FavouriteRoute.FavouriteRoute)
      :> ReqBody
           ('[JSON])
           API.Types.UI.FavouriteRoute.UpdateFavouriteRouteReq
      :> Put
           ('[JSON])
           API.Types.UI.FavouriteRoute.FavouriteRouteAPIEntity
      :<|> TokenAuth
      :> "favouriteRoute"
      :> Capture
           "favouriteId"
           (Kernel.Types.Id.Id Domain.Types.FavouriteRoute.FavouriteRoute)
      :> Delete
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postFavouriteRouteCreate :<|> getFavouriteRouteList :<|> putFavouriteRoute :<|> deleteFavouriteRoute

postFavouriteRouteCreate ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.FavouriteRoute.CreateFavouriteRouteReq ->
    Environment.FlowHandler API.Types.UI.FavouriteRoute.FavouriteRouteAPIEntity
  )
postFavouriteRouteCreate a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FavouriteRoute.postFavouriteRouteCreate (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getFavouriteRouteList ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.FlowHandler [API.Types.UI.FavouriteRoute.FavouriteRouteAPIEntity]
  )
getFavouriteRouteList a1 = withFlowHandlerAPI $ Domain.Action.UI.FavouriteRoute.getFavouriteRouteList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

putFavouriteRoute ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.FavouriteRoute.FavouriteRoute ->
    API.Types.UI.FavouriteRoute.UpdateFavouriteRouteReq ->
    Environment.FlowHandler API.Types.UI.FavouriteRoute.FavouriteRouteAPIEntity
  )
putFavouriteRoute a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FavouriteRoute.putFavouriteRoute (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

deleteFavouriteRoute ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.FavouriteRoute.FavouriteRoute ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
deleteFavouriteRoute a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FavouriteRoute.deleteFavouriteRoute (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
