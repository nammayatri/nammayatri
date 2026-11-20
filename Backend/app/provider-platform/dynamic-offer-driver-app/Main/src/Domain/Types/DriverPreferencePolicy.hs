{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Generic driver preference locks a fleet can set (fleet_config.preference_policy), v1: validation only.
-- See SharedLogic.DriverPreferencePolicy for how they are enforced.
--
-- The JSON is aeson's generic encoding (TaggedObject tag/contents for 'PreferenceValue'); it is both the wire format
-- of /fleet/config and the stored jsonb, so keep constructor and field names stable.
module Domain.Types.DriverPreferencePolicy
  ( DriverPreference (..),
    PreferenceValue (..),
    PreferenceLock (..),
    PreferencePolicy (..),
  )
where

import Kernel.Prelude

data DriverPreference = ServiceTier | AirConditioned | IntraCity | InterCity | Rental | TripDistance | AreaPreference
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data PreferenceValue = ToggleValue Bool | AutoAssignTiers
  deriving (Generic, FromJSON, ToJSON, Eq, Show, ToSchema)

-- | value Nothing = frozen at the driver's current value.
data PreferenceLock = PreferenceLock {preference :: DriverPreference, value :: Maybe PreferenceValue}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

data PreferencePolicy = PreferencePolicy {hidden :: [DriverPreference], locked :: [PreferenceLock]}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)
