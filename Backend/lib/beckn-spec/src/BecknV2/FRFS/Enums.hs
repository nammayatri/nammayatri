{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module BecknV2.FRFS.Enums where

import Data.Aeson
import Data.Aeson.Types
import Kernel.Prelude
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.GenericPretty
import Kernel.Utils.JSON

data Domain
  = FRFS
  deriving (Eq, Generic, Show, Read, FromDhall, ToSchema)
  deriving (PrettyShow) via Showable Domain

instance FromJSON Domain where
  parseJSON (String "ONDC:TRV11") = pure FRFS
  parseJSON (String _) = parseFail "Invalid Domain"
  parseJSON e = typeMismatch "String" e

instance ToJSON Domain where
  toJSON FRFS = String "ONDC:TRV11"

-- toJSON _ = error "Invalid Domain"

data Action
  = SEARCH
  | SELECT
  | INIT
  | CONFIRM
  | UPDATE
  | STATUS
  | TRACK
  | CANCEL
  | RATING
  | SUPPORT
  | ON_SEARCH
  | ON_SELECT
  | ON_INIT
  | ON_CONFIRM
  | ON_UPDATE
  | ON_STATUS
  | ON_TRACK
  | ON_CANCEL
  | ON_RATING
  | ON_SUPPORT
  deriving (Generic, Show, Eq, ToSchema)
  deriving (PrettyShow) via Showable Action

instance FromJSON Action where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON Action where
  toJSON = genericToJSON constructorsToLowerOptions

data VehicleCategory = METRO
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

data StopType = START | END | INTERMEDIATE_STOP
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)
