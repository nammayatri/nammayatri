{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.RewardContext
  ( RewardContext (..),
    RewardRideInput (..),
    noRideInput,
    defaultRewardContext,
    rewardContextToLogicInput,
    rewardContextKeys,
    rideScopedContextKeys,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Domain.Types.ServiceTierType as DST
import qualified Domain.Types.VehicleVariant as DVV
import Kernel.Prelude

-- | Type-safe rider context for reward-cohort eligibility JSON logic.
--
-- All fields are optional so callers (e.g. the dashboard validation API) can
-- supply a partial context; absent values are defaulted when the context is
-- serialized for evaluation (see 'rewardContextToLogicInput') so a logic always
-- runs against concrete values. The same type builds the production context in
-- @Tools.Rewards.RiderContextReader.readRiderContext@, keeping the context
-- shape a single source of truth that the validation tool cannot drift from.
data RewardContext = RewardContext
  { ridesLast1d :: Maybe Int,
    ridesLast3d :: Maybe Int,
    ridesLast7d :: Maybe Int,
    ridesLast30d :: Maybe Int,
    ridesLast90d :: Maybe Int,
    hasTakenValidRide :: Maybe Bool,
    isValidRide :: Maybe Bool,
    -- | Vehicle variant of the ride that triggered the evaluation (e.g.
    -- @"AUTO_RICKSHAW"@, @"SEDAN"@). Absent when there is no ride context.
    rideVehicleVariant :: Maybe Text,
    -- | Service tier of the ride that triggered the evaluation (e.g. @"COMFY"@,
    -- @"ECO"@). Absent when there is no ride context.
    rideServiceTier :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

-- | Facts about the ride that triggered a reward evaluation. Every field is
-- 'Nothing' when there is no ride (dashboard triggerEval, home-screen live card).
data RewardRideInput = RewardRideInput
  { isValid :: Maybe Bool,
    vehicleVariant :: Maybe DVV.VehicleVariant,
    serviceTier :: Maybe DST.ServiceTierType
  }
  deriving (Generic, Show, Eq)

-- | No ride context: evaluation not triggered by a ride.
noRideInput :: RewardRideInput
noRideInput = RewardRideInput {isValid = Nothing, vehicleVariant = Nothing, serviceTier = Nothing}

-- | Empty context: every field absent. Used when a request omits @context@.
defaultRewardContext :: RewardContext
defaultRewardContext =
  RewardContext
    { ridesLast1d = Nothing,
      ridesLast3d = Nothing,
      ridesLast7d = Nothing,
      ridesLast30d = Nothing,
      ridesLast90d = Nothing,
      hasTakenValidRide = Nothing,
      isValidRide = Nothing,
      rideVehicleVariant = Nothing,
      rideServiceTier = Nothing
    }

-- | The single serializer feeding JSON logic (production AND dashboard tool).
--
-- Applies defaults for absent fields (windowed counts -> 0,
-- @hasTakenValidRide@ -> False). The ride-scoped fields (@isValidRide@,
-- @rideVehicleVariant@, @rideServiceTier@) stay @null@ when absent, matching the
-- production "no ride context" semantics — do not default them. Existing keys
-- serialize exactly as before, so existing cohort logics are unaffected.
rewardContextToLogicInput :: RewardContext -> A.Value
rewardContextToLogicInput RewardContext {..} =
  A.object
    [ "ridesLast1d" A..= fromMaybe 0 ridesLast1d,
      "ridesLast3d" A..= fromMaybe 0 ridesLast3d,
      "ridesLast7d" A..= fromMaybe 0 ridesLast7d,
      "ridesLast30d" A..= fromMaybe 0 ridesLast30d,
      "ridesLast90d" A..= fromMaybe 0 ridesLast90d,
      "hasTakenValidRide" A..= fromMaybe False hasTakenValidRide,
      "isValidRide" A..= isValidRide,
      "rideVehicleVariant" A..= rideVehicleVariant,
      "rideServiceTier" A..= rideServiceTier
    ]

-- | The context keys a cohort eligibility logic may reference. Derived from the
-- serialized default so it can never drift from what 'rewardContextToLogicInput'
-- actually feeds to the evaluator. Used to reject logics that reference fields
-- outside the typed context.
rewardContextKeys :: [Text]
rewardContextKeys = case rewardContextToLogicInput defaultRewardContext of
  A.Object o -> AK.toText <$> AKM.keys o
  _ -> []

-- | Context keys that describe the ride triggering an evaluation. They are
-- always null where there is no ride (e.g. the home-screen live card), so logics
-- evaluated there must not reference them.
rideScopedContextKeys :: [Text]
rideScopedContextKeys = ["isValidRide", "rideVehicleVariant", "rideServiceTier"]
