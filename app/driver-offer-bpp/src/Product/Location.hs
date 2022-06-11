module Product.Location where

import Beckn.Prelude
import Beckn.Storage.Esqueleto (Transactionable (runTransaction))
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Logging
import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.Person as Person
import Environment
import GHC.Records.Extra
import Storage.Queries.DriverLocation (upsertGpsCoord)
import qualified Storage.Queries.Person as Person
import Types.API.Location as Location
import Utils.Common hiding (id)

updateLocation :: Id Person.Person -> UpdateLocationReq -> FlowHandler APISuccess
updateLocation driverId waypoints = withFlowHandlerAPI $
  withLogTag "driverLocationUpdate" $ do
    logInfo $ "got location updates: " <> getId driverId <> " " <> encodeToText waypoints
    driver <-
      Person.findById driverId
        >>= fromMaybeM (PersonNotFound driverId.getId)
    unless (driver.role == Person.DRIVER) $ throwError AccessDenied
    let currPoint = NE.last waypoints
    runTransaction $ upsertGpsCoord driver.id currPoint.pt currPoint.ts
    pure Success
