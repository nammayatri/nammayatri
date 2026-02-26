{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module BecknV2.OnDemand.Utils.Version
  ( SpecVersion (..),
    parseSpecVersion,
    isV21,
    isV20,
    getContextVersion,
    supportedVersions,
  )
where

import Data.Text (Text)
import qualified BecknV2.OnDemand.Types as Spec
import Prelude

-- | Supported Beckn protocol spec versions
data SpecVersion = V2_0_0 | V2_1_0
  deriving (Show, Eq, Ord)

-- | Parse a version string to SpecVersion
parseSpecVersion :: Text -> SpecVersion
parseSpecVersion "2.1.0" = V2_1_0
parseSpecVersion _ = V2_0_0 -- default to 2.0.0 for backward compat

-- | Get the spec version from a Context object
getContextVersion :: Spec.Context -> SpecVersion
getContextVersion ctx = case Spec.contextVersion ctx of
  Just v -> parseSpecVersion v
  Nothing -> V2_0_0

-- | Check if the context indicates v2.1.0
isV21 :: Spec.Context -> Bool
isV21 = (== V2_1_0) . getContextVersion

-- | Check if the context indicates v2.0.0
isV20 :: Spec.Context -> Bool
isV20 = (== V2_0_0) . getContextVersion

-- | All supported versions
supportedVersions :: [Text]
supportedVersions = ["2.0.0", "2.1.0"]
