{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.ARDUBPPMetrics
  ( module Tools.Metrics.ARDUBPPMetrics,
    module Reexport,
  )
where

import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.Records.Extra
import Kernel.Types.Common
import Kernel.Utils.Time (getClockTimeInMs)
import Prometheus as P
import Tools.Metrics.ARDUBPPMetrics.Types as Reexport

putFareAndDistanceDeviations :: (MonadIO m, HasBPPMetrics m r) => Text -> Money -> Meters -> m ()
putFareAndDistanceDeviations agencyName fareDiff distanceDiff = do
  countingDeviationMetric <- asks (.bppMetrics.countingDeviation)
  version <- asks (.version)
  liftIO $ P.withLabel countingDeviationMetric.realFareDeviation (agencyName, version.getDeploymentVersion) (`P.observe` fromIntegral fareDiff)
  liftIO $ P.withLabel countingDeviationMetric.realDistanceDeviation (agencyName, version.getDeploymentVersion) (`P.observe` fromIntegral distanceDiff)

type RequestMetricsMVar = MVar Milliseconds

startSearchMetrics :: HasBPPMetrics m r => Text -> m RequestMetricsMVar
startSearchMetrics agencyName = do
  bmContainer <- asks (.bppMetrics)
  version <- asks (.version)
  startRequestMetrics agencyName version bmContainer.searchDurationTimeout bmContainer.searchDuration "Search"

finishSearchMetrics :: HasBPPMetrics m r => Text -> RequestMetricsMVar -> m ()
finishSearchMetrics agencyName requestMetricsMVar = do
  bmContainer <- asks (.bppMetrics)
  version <- asks (.version)
  finishRequestMetrics agencyName version bmContainer.searchDuration requestMetricsMVar

startSelectMetrics :: HasBPPMetrics m r => Text -> m RequestMetricsMVar
startSelectMetrics agencyName = do
  bmContainer <- asks (.bppMetrics)
  version <- asks (.version)
  startRequestMetrics agencyName version bmContainer.selectDurationTimeout bmContainer.selectDuration "Select"

finishSelectMetrics :: HasBPPMetrics m r => Text -> RequestMetricsMVar -> m ()
finishSelectMetrics agencyName requestMetricsMVar = do
  bmContainer <- asks (.bppMetrics)
  version <- asks (.version)
  finishRequestMetrics agencyName version bmContainer.selectDuration requestMetricsMVar

startRequestMetrics :: MonadFlow m => Text -> DeploymentVersion -> Seconds -> RequestDurationMetric -> Text -> m RequestMetricsMVar
startRequestMetrics agencyName version (Seconds requestDurationTimeout) requestDuration requestName = do
  let (_, failureCounter) = requestDuration
  startTime <- getClockTimeInMs
  requestMetricsMVar <- liftIO $ newMVar startTime
  fork ("BPP " <> requestName <> " Metrics") $ do
    liftIO $ threadDelay $ requestDurationTimeout * 1000000
    whenJustM (liftIO $ tryTakeMVar requestMetricsMVar) $ \_ -> do
      liftIO $ P.withLabel failureCounter (agencyName, version.getDeploymentVersion) P.incCounter
  return requestMetricsMVar

finishRequestMetrics ::
  MonadFlow m =>
  Text ->
  DeploymentVersion ->
  RequestDurationMetric ->
  RequestMetricsMVar ->
  m ()
finishRequestMetrics agencyName version requestDuration requestMetricsMVar = do
  let (requestDurationHistogram, _) = requestDuration
  whenJustM (liftIO $ tryTakeMVar requestMetricsMVar) $ \startTime -> do
    endTime <- getClockTimeInMs
    putRequestDuration agencyName version requestDurationHistogram $ fromIntegral $ endTime - startTime

putRequestDuration :: L.MonadFlow m => Text -> DeploymentVersion -> P.Vector P.Label2 P.Histogram -> Double -> m ()
putRequestDuration agencyName version requestDurationHistogram duration =
  L.runIO $
    P.withLabel
      requestDurationHistogram
      (agencyName, version.getDeploymentVersion)
      (`P.observe` duration)
