module SharedLogic.WMB where

import API.Types.UI.WMB
import Data.Time hiding (getCurrentTime)
import Domain.Types.Person
import Domain.Types.Route
import Domain.Types.TripTransaction
import Environment
import qualified EulerHS.Prelude as EHS
import Kernel.Prelude
import Kernel.Types.Id
import qualified Kernel.Utils.CalculateDistance as KU
import Kernel.Utils.Common
import qualified Storage.Queries.FleetDriverAssociation as QFDV
import qualified Storage.Queries.RouteTripStopMapping as QRTS
import qualified Storage.Queries.TripTransaction as QTT
import Tools.Error

checkFleetDriverAssociation :: Id Person -> Id Person -> Flow Bool
checkFleetDriverAssociation driverId fleetOwnerId = do
  mbFleetDriverAssociation <- QFDV.findByDriverId driverId True
  case mbFleetDriverAssociation of
    Nothing -> return False
    Just fleetDriverAssociation -> return $ fleetDriverAssociation.isActive && fleetDriverAssociation.fleetOwnerId == fleetOwnerId.getId

getSourceAndDestinationStopInfo :: Route -> Text -> Flow (StopInfo, StopInfo)
getSourceAndDestinationStopInfo route routeCode = do
  now <- getCurrentTime
  allRTSList <- QRTS.findAllRTSMappingByRouteAndDay routeCode (utctTimeToDayOfWeek now)
  nonEmptyAllRTSList <-
    case allRTSList of
      [] -> throwError $ InvalidRequest "RTS not found"
      (a : as) -> pure $ a :| as
  let sourceRouteTripMapping = minimumBy (EHS.comparing (\r -> KU.distanceBetweenInMeters route.startPoint r.stopPoint)) nonEmptyAllRTSList
      destinationRouteTripMapping = minimumBy (EHS.comparing (\r -> KU.distanceBetweenInMeters route.endPoint r.stopPoint)) nonEmptyAllRTSList
  pure (createStopInfo sourceRouteTripMapping route.startPoint, createStopInfo destinationRouteTripMapping route.endPoint)
  where
    createStopInfo routeTripMapping point =
      StopInfo
        { name = routeTripMapping.stopName,
          code = routeTripMapping.stopCode,
          ..
        }

    utctTimeToDayOfWeek :: UTCTime -> DayOfWeek
    utctTimeToDayOfWeek utcTime = dayOfWeek (utctDay utcTime)

findNextEligibleTripTransactionByDriverIdStatus :: Id Person -> TripStatus -> Flow (Maybe TripTransaction)
findNextEligibleTripTransactionByDriverIdStatus driverId status = do
  let sortType = if isNonTerminalTripStatus status then QTT.SortAsc else QTT.SortDesc
  QTT.findAllTripTransactionByDriverIdStatus driverId (Just 1) (Just 0) (Just status) sortType >>= \case
    (trip : _) -> pure (Just trip)
    [] -> pure Nothing

isNonTerminalTripStatus :: TripStatus -> Bool
isNonTerminalTripStatus status = any (\status' -> status' == status) [TRIP_ASSIGNED, IN_PROGRESS]
