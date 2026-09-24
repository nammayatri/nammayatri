module Lib.Yudhishthira.Tools.TimeBoundRollout
  ( unboundedTimeBound,
    TimeBoundUtcOffsetKey (..),
    defaultTimeBoundUtcOffset,
    getTimeBoundLocalTime,
    activeTimeBoundName,
    applicableForTimeBound,
    filterByActiveTimeBound,
  )
where

import qualified EulerHS.Language as L
import EulerHS.Types (OptionEntity)
import Kernel.Prelude
import Kernel.Types.Common (Seconds (..))
import Kernel.Types.Id (Id, getId)
import Kernel.Types.TimeBound (TimeBound (..), findBoundedDomain)
import Kernel.Utils.Common (getLocalCurrentTime, logDebug)
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Yudhishthira.Storage.CachedQueries.TimeBoundConfig as CTBC
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.AppDynamicLogicRollout (AppDynamicLogicRollout)

unboundedTimeBound :: Text
unboundedTimeBound = "Unbounded"

data TimeBoundUtcOffsetKey = TimeBoundUtcOffsetKey
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity TimeBoundUtcOffsetKey Seconds

defaultTimeBoundUtcOffset :: Seconds
defaultTimeBoundUtcOffset = Seconds 19800

getTimeBoundLocalTime :: BeamFlow.BeamFlow m r => m UTCTime
getTimeBoundLocalTime = do
  mbOffset <- L.getOptionLocal TimeBoundUtcOffsetKey
  getLocalCurrentTime (fromMaybe defaultTimeBoundUtcOffset mbOffset)

activeTimeBoundName ::
  BeamFlow.BeamFlow m r =>
  Id LYT.MerchantOperatingCity ->
  LYT.LogicDomain ->
  UTCTime ->
  m (Maybe Text)
activeTimeBoundName merchantOpCityId domain localTime = do
  windows <- CTBC.findByCityAndDomain merchantOpCityId domain
  let namedWindows = filter (\cfg -> cfg.timeBounds /= Unbounded) windows
  return $ (.name) <$> listToMaybe (findBoundedDomain namedWindows localTime)

applicableForTimeBound :: Maybe Text -> [AppDynamicLogicRollout] -> [AppDynamicLogicRollout]
applicableForTimeBound mbWindowName rollouts =
  case mbWindowName of
    Nothing -> unboundedRollouts
    Just windowName ->
      case filter (\r -> r.timeBounds == windowName) rollouts of
        [] -> unboundedRollouts
        boundedRollouts -> boundedRollouts
  where
    unboundedRollouts = filter (\r -> r.timeBounds == unboundedTimeBound) rollouts

filterByActiveTimeBound ::
  BeamFlow.BeamFlow m r =>
  Id LYT.MerchantOperatingCity ->
  LYT.LogicDomain ->
  [AppDynamicLogicRollout] ->
  m [AppDynamicLogicRollout]
filterByActiveTimeBound merchantOpCityId domain rollouts
  | all (\r -> r.timeBounds == unboundedTimeBound) rollouts = return rollouts
  | otherwise = do
    localTime <- getTimeBoundLocalTime
    mbWindowName <- activeTimeBoundName merchantOpCityId domain localTime
    let applicable = applicableForTimeBound mbWindowName rollouts
    logDebug $
      "TIME_BOUND_ROLLOUT: domain=" <> show domain <> " city=" <> getId merchantOpCityId
        <> " localTime="
        <> show localTime
        <> " window="
        <> show mbWindowName
        <> " applicableVersions="
        <> show (map (.version) applicable)
    return applicable
