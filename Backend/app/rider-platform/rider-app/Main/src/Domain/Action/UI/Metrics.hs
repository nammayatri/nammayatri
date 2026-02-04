module Domain.Action.UI.Metrics (postMetricsIncrement) where

import qualified API.Types.UI.Metrics
import qualified Data.Text as T
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions (runInReplica)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common (fork, logError)
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Queries.Person as QPerson
import Text.Regex.Posix ((=~))
import Tools.Metrics

-- Process metrics in background
processMetrics ::
  Maybe (Id Domain.Types.Person.Person) ->
  Text ->
  Text ->
  Environment.Flow ()
processMetrics mbPersonId metricName message = do
  blacklistPatterns <- case mbPersonId of
    Just personId -> do
      mbPerson <- runInReplica $ QPerson.findById personId
      case mbPerson of
        Just person -> do
          mbRiderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId, txnId = Nothing})
          pure $ case mbRiderConfig of
            Just riderConfig -> fromMaybe [] riderConfig.metricsBlacklistPatterns
            Nothing -> []
        Nothing -> pure []
    Nothing -> pure []

  let isBlacklisted = Kernel.Prelude.any (\blacklistPattern -> (T.unpack message =~ T.unpack blacklistPattern :: Bool)) blacklistPatterns

  unless isBlacklisted $ do
    logError $ "FRONTEND_METRIC | metric: " <> metricName <> " | message: " <> message
    incrementGenericMetrics' metricName

postMetricsIncrement ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.Metrics.MetricCounterReq ->
    Environment.Flow APISuccess.APISuccess
  )
postMetricsIncrement (mbPersonId, _) (API.Types.UI.Metrics.MetricCounterReq {..}) = do
  fork "Process Metrics" $ processMetrics mbPersonId metricName message
  return APISuccess.Success
