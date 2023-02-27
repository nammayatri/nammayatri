{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Metro.Search.Image (Image (..)) where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype Image = Image Text
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

-- TODO: functions to work with different formats
-- https://raw.githubusercontent.com/beckn/protocol-specifications/core-v0.9.1/core/v0/schema/image.json
