module Product.Ride where

import App.Types
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.ProductInstance as MPI
import qualified Types.API.Ride as API
import Types.Error
import qualified Types.Storage.Person as SPerson
import qualified Types.Storage.ProductInstance as SPI
import Utils.Common

getDriverLoc :: Id SPI.ProductInstance -> Id SPerson.Person -> FlowHandler API.GetDriverLocRes
getDriverLoc rideId personId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  baseUrl <- xProviderUri <$> ask
  orderPI <- MPI.findById rideId >>= fromMaybeM PIDoesNotExist
  orderPIid <- orderPI.parentId & fromMaybeM (PIFieldNotPresent "parentId")
  res <- ExternalAPI.location baseUrl (getId orderPIid)
  return $ makeGetDriverLocRes res.currPoint
  where
    makeGetDriverLocRes LatLong {..} =
      API.GetDriverLocRes {long = lon, ..}