{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.Maps where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Action.UI.Maps
import qualified "this" Domain.Types.Person
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

type API = ("maps" :> (PostMapsAutoComplete :<|> PostMapsGetPlaceName))

type PostMapsAutoComplete =
  ( "autoComplete" :> Capture "driverId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> ReqBody '[JSON] Domain.Action.UI.Maps.AutoCompleteReq
      :> Post
           '[JSON]
           Domain.Action.UI.Maps.AutoCompleteResp
  )

type PostMapsGetPlaceName =
  ( "getPlaceName" :> Capture "driverId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> ReqBody '[JSON] Domain.Action.UI.Maps.GetPlaceNameReq
      :> Post
           '[JSON]
           Domain.Action.UI.Maps.GetPlaceNameResp
  )

data MapsAPIs = MapsAPIs
  { postMapsAutoComplete :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Maps.AutoCompleteReq -> EulerHS.Types.EulerClient Domain.Action.UI.Maps.AutoCompleteResp,
    postMapsGetPlaceName :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Maps.GetPlaceNameReq -> EulerHS.Types.EulerClient Domain.Action.UI.Maps.GetPlaceNameResp
  }

mkMapsAPIs :: (Client EulerHS.Types.EulerClient API -> MapsAPIs)
mkMapsAPIs mapsClient = (MapsAPIs {..})
  where
    postMapsAutoComplete :<|> postMapsGetPlaceName = mapsClient

data MapsUserActionType
  = POST_MAPS_AUTO_COMPLETE
  | POST_MAPS_GET_PLACE_NAME
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''MapsUserActionType])
