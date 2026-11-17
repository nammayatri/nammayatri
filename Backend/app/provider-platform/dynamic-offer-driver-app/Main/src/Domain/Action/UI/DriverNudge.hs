module Domain.Action.UI.DriverNudge
  ( postInternalNudgeStart,
    postInternalNudgeEnd,
    postInternalNudgeBulkLocUpdate,
    postDriverDemandHotspotResponse,
  )
where

import qualified API.Types.UI.DriverNudge
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Types.DriverNudge as DDriverNudge
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Environment
import EulerHS.Prelude hiding (any, id, map, whenJust)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common (fromMaybeM, throwError)
import Lib.LocationUpdates (addIntermediateRoutePoints, buildNudgeInterpolationHandler)
import qualified SharedLogic.External.LocationTrackingService.Flow as LTS
import SharedLogic.InternalAuth (checkLtsKey)
import qualified Storage.Queries.DriverNudge as QDriverNudge

postDriverDemandHotspotResponse ::
  ( Maybe (Id Person),
    Id Merchant,
    Id MerchantOperatingCity
  ) ->
  API.Types.UI.DriverNudge.DemandNudgeResponseReq ->
  Flow APISuccess
postDriverDemandHotspotResponse (mbDriverId, merchantId, _merchantOpCityId) req = do
  driverId <- fromMaybeM (InvalidRequest "Driver not logged in") mbDriverId
  nudge <- QDriverNudge.findById req.nudgeId >>= fromMaybeM (InvalidRequest $ "DriverNudge not found: " <> req.nudgeId.getId)
  unless (nudge.driverId == driverId) $ throwError (InvalidRequest "Nudge does not belong to this driver")
  when (nudge.status == DDriverNudge.PENDING) $
    case req.status of
      API.Types.UI.DriverNudge.ACCEPTED -> do
        whenJust nudge.destination $ \destination ->
          void $ LTS.nudgeStart nudge.id merchantId driverId destination
        QDriverNudge.updateStatus DDriverNudge.ACTIVE nudge.id
      API.Types.UI.DriverNudge.DECLINED ->
        QDriverNudge.updateStatus DDriverNudge.CANCELLED nudge.id
  pure Success

postInternalNudgeStart ::
  Text ->
  Maybe Text ->
  API.Types.UI.DriverNudge.NudgeStartReq ->
  Environment.Flow APISuccess
postInternalNudgeStart nudgeIdText _mbApiKey _req = do
  let nudgeId = Id nudgeIdText :: Id DDriverNudge.DriverNudge
  nudge <- QDriverNudge.findById nudgeId >>= fromMaybeM (InvalidRequest $ "DriverNudge not found: " <> nudgeIdText)
  when (nudge.status == DDriverNudge.ACTIVE) $
    QDriverNudge.updateStatus DDriverNudge.ACTIVE nudge.id
  pure Success

postInternalNudgeEnd ::
  Text ->
  Maybe Text ->
  API.Types.UI.DriverNudge.NudgeEndReq ->
  Environment.Flow APISuccess
postInternalNudgeEnd nudgeIdText _mbApiKey _req = do
  let nudgeId = Id nudgeIdText :: Id DDriverNudge.DriverNudge
  nudge <- QDriverNudge.findById nudgeId >>= fromMaybeM (InvalidRequest $ "DriverNudge not found: " <> nudgeIdText)
  when (nudge.status == DDriverNudge.ACTIVE) $
    QDriverNudge.updateStatus DDriverNudge.COMPLETED nudge.id
  pure Success

postInternalNudgeBulkLocUpdate ::
  Text ->
  Maybe Text ->
  API.Types.UI.DriverNudge.NudgeBulkLocUpdateReq ->
  Environment.Flow APISuccess
postInternalNudgeBulkLocUpdate nudgeIdText mbApiKey req = do
  checkLtsKey "nudgeBulkLocUpdate" mbApiKey
  let nudgeId = Id nudgeIdText :: Id DDriverNudge.DriverNudge
      driverId = req.driverId
  nudge <- QDriverNudge.findById nudgeId >>= fromMaybeM (InvalidRequest $ "DriverNudge not found: " <> nudgeIdText)
  when (nudge.status == DDriverNudge.ACTIVE) $ do
    let accLoc = filter (\w -> maybe True (< 50.0) w.acc) req.loc
        accPoints = map (\w -> LatLong w.lat w.lon) accLoc
    whenJust (nonEmpty req.loc) $ \allLocNE -> do
      now <- getCurrentTime
      let nowTs = floor $ utcTimeToPOSIXSeconds now
          mkWp w = (LatLong w.lat w.lon, fromMaybe nowTs w.ts)
          accWaypointsWithTime = map mkWp accLoc
          allWaypointsWithTime = fmap mkWp allLocNE
      ih <-
        buildNudgeInterpolationHandler nudge.merchantOperatingCityId
          nudgeIdText
          Nothing
          (\dist failed -> unless failed $ QDriverNudge.updateActualDistance (Just dist) nudgeId)
      addIntermediateRoutePoints ih Nothing False False nudgeId driverId False False accWaypointsWithTime allWaypointsWithTime
    let nudgeRadius = metersToHighPrecMeters (Meters 100)
        reached =
          maybe
            False
            (\dest -> any (\pt -> distanceBetweenInMeters pt dest <= nudgeRadius) accPoints)
            nudge.destination
    when reached $ do
      void $ LTS.nudgeEnd nudgeId nudge.merchantId driverId
      QDriverNudge.updateStatus DDriverNudge.COMPLETED nudgeId
  pure Success
