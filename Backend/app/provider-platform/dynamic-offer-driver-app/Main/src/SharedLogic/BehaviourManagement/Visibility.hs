{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.BehaviourManagement.Visibility
  ( getDriverBehaviorVisibility,
    getRiderBehaviorVisibility,
    getEntityBehaviorVisibility,
    defaultDriverDomainConfig,
    defaultRiderDomainConfig,
  )
where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import qualified Lib.BehaviorTracker.Types as BTT
import qualified Lib.BehaviorTracker.Visibility as BTV

-- | Default domain config for driver-app visibility.
defaultDriverDomainConfig :: BTT.BehaviorDomainConfig
defaultDriverDomainConfig =
  BTT.BehaviorDomainConfig
    { actionTypes =
        [ ( "GPS_TOLL_BAD_BEHAVIOR",
            BTT.CounterConfig
              { windowSizeDays = 30,
                counters = [BTT.ACTION_COUNT],
                periods = [BTT.mkPeriodConfig "window" 15]
              }
          ),
          ( "RIDE_CANCELLATION",
            BTT.CounterConfig
              { windowSizeDays = 7,
                counters = [BTT.ACTION_COUNT, BTT.ELIGIBLE_COUNT],
                periods =
                  [ BTT.mkPeriodConfig "daily" 1,
                    BTT.mkPeriodConfig "weekly" 7
                  ]
              }
          )
        ],
      blockReasonTags =
        [ "TOLL_ROUTES",
          "CancellationRateDaily",
          "CancellationRateWeekly",
          "ExtraFareDaily",
          "ExtraFareWeekly",
          "DrunkAndDriveViolation",
          "HARD_BLOCK",
          "SOFT_BLOCK",
          "PERMANENT_BLOCK"
        ],
      blockTypes = [BTT.HARD_BLOCK, BTT.SOFT_BLOCK, BTT.FEATURE_BLOCK, BTT.PERMANENT_BLOCK]
    }

-- | Default domain config for rider-app visibility.
defaultRiderDomainConfig :: BTT.BehaviorDomainConfig
defaultRiderDomainConfig =
  BTT.BehaviorDomainConfig
    { actionTypes = [],
      blockReasonTags = [],
      blockTypes = [BTT.HARD_BLOCK, BTT.SOFT_BLOCK, BTT.FEATURE_BLOCK, BTT.PERMANENT_BLOCK]
    }

-- | Pick default config based on entity type
defaultConfigForEntity :: BTT.EntityType -> BTT.BehaviorDomainConfig
defaultConfigForEntity BTT.DRIVER = defaultDriverDomainConfig
defaultConfigForEntity BTT.RIDER = defaultRiderDomainConfig

-- | Get full behavior visibility for a driver
getDriverBehaviorVisibility ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  Text -> -- driverId
  m BTT.EntityBehaviorVisibility
getDriverBehaviorVisibility driverId =
  BTV.queryEntityVisibility BTT.DRIVER driverId defaultDriverDomainConfig

-- | Get full behavior visibility for a rider
getRiderBehaviorVisibility ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  Text -> -- riderId
  m BTT.EntityBehaviorVisibility
getRiderBehaviorVisibility riderId =
  BTV.queryEntityVisibility BTT.RIDER riderId defaultRiderDomainConfig

-- | Generic: get behavior visibility for any entity type
getEntityBehaviorVisibility ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  BTT.EntityType ->
  Text -> -- entityId
  Maybe BTT.BehaviorDomainConfig -> -- custom config, defaults based on entityType
  m BTT.EntityBehaviorVisibility
getEntityBehaviorVisibility entityType entityId mbConfig =
  BTV.queryEntityVisibility entityType entityId (fromMaybe (defaultConfigForEntity entityType) mbConfig)
