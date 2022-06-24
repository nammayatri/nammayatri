module Product.Location where

import App.Types
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import Beckn.Types.Id
import qualified Domain.Types.Person as Person
import EulerHS.Prelude
import qualified Types.API.Location as Location
import Utils.Common


getRoute' :: Id Person.Person -> Location.Request -> FlowHandler Location.Response
getRoute' personId = withFlowHandlerAPI . withPersonIdLogTag personId . GoogleMaps.getRoutes'