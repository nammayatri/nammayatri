 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Metro.Search.Vehicle where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data Vehicle = Vehicle
  { category :: Maybe Text,
    capacity :: Maybe Int,
    make :: Maybe Text,
    model :: Maybe Text,
    size :: Maybe Text,
    variant :: Maybe Text,
    color :: Maybe Text,
    energy_type :: Maybe Text,
    registration :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
