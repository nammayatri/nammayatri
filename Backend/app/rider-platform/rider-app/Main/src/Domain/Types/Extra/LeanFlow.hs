{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Extra.LeanFlow where

import Data.Aeson (withObject, (.!=), (.:?))
import qualified Data.Aeson.Types as A
import Kernel.Prelude

data LeanFlowFeature
  = WALK_AND_SAVE
  | HOTSPOT
  | REWARD_INFLIGHT_RECONCILE
  | FRFS_SEAT_HOLD_REAPER
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data LeanFlowConfig = LeanFlowConfig
  { enabled :: Bool,
    featuresExcluded :: [LeanFlowFeature]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON LeanFlowConfig where
  parseJSON = withObject "LeanFlowConfig" $ \o -> do
    enabled <- o .:? "enabled" .!= False
    rawFeatures <- o .:? "featuresExcluded" .!= []
    let featuresExcluded = [f | v <- rawFeatures, Just f <- [A.parseMaybe A.parseJSON v]]
    pure LeanFlowConfig {enabled, featuresExcluded}
