 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverPool.Config where

import Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Kernel.Utils.Dhall (FromDhall)

data IntelligentPoolConfig = IntelligentPoolConfig
  { minQuotesToQualifyForIntelligentPool :: Int,
    minQuotesToQualifyForIntelligentPoolWindowOption :: SWC.SlidingWindowOptions
  }
  deriving (Generic, FromDhall)

data CancellationScoreRelatedConfig = CancellationScoreRelatedConfig
  { popupDelayToAddAsPenalty :: Maybe Seconds,
    thresholdCancellationScore :: Maybe Int,
    thresholdRidesCount :: Maybe Int
  }
  deriving (Generic, FromDhall)

data DriverPoolConfig = DriverPoolConfig
  { minRadiusOfSearch :: Meters,
    maxRadiusOfSearch :: Meters,
    radiusStepSize :: Meters,
    driverPositionInfoExpiry :: Maybe Seconds,
    actualDistanceThreshold :: Maybe Meters,
    maxDriverQuotesRequired :: Int,
    driverQuoteLimit :: Int,
    intelligentPoolPercentage :: Maybe Int
  }
  deriving (Generic, FromDhall)

data OverrideDriverPoolConfigRange = OverrideDriverPoolConfigRange
  { startDistance :: Meters,
    endDistance :: Maybe Meters
  }
  deriving (Generic, FromDhall)

data OverrideDriverPoolConfig = OverrideDriverPoolConfig
  { configRange :: OverrideDriverPoolConfigRange,
    driverPoolCfg :: DriverPoolConfig
  }
  deriving (Generic, FromDhall)

type HasDriverPoolConfig r =
  ( HasField "driverPoolCfg" r DriverPoolConfig,
    HasField "overrideDriverPoolConfig" r [OverrideDriverPoolConfig],
    HasField "intelligentPoolConfig" r IntelligentPoolConfig,
    HasField "cancellationScoreRelatedConfig" r CancellationScoreRelatedConfig,
    HasField "defaultPopupDelay" r Seconds,
    HasMaxParallelSearchRequests r
  )

type HasMaxParallelSearchRequests r =
  ( HasField "maxParallelSearchRequests" r Int
  )

getDriverPoolConfig :: (MonadFlow m, MonadReader r m, HasDriverPoolConfig r) => Meters -> m DriverPoolConfig
getDriverPoolConfig dist = do
  defaultConfig <- asks (.driverPoolCfg)
  overrideConfigs <- asks (.overrideDriverPoolConfig)
  let applicableConfig = find filterByDist overrideConfigs
  return $ maybe defaultConfig (.driverPoolCfg) applicableConfig
  where
    filterByDist cfg =
      (dist >= cfg.configRange.startDistance)
        && maybe True (dist <=) cfg.configRange.endDistance
