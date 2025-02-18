{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FRFSRouteDetails where

import Data.Aeson
import Data.OpenApi hiding (name)
import qualified Data.Text as T
import EulerHS.Prelude hiding (length, map, readMaybe)

data FRFSRouteDetails = FRFSRouteDetails
  { startStationCode :: Text,
    endStationCode :: Text,
    routeCode :: Maybe Text
  }
  deriving (Generic, Show, Read, Eq, Ord, FromJSON, ToJSON, ToSchema)

gtfsIdtoDomainCode :: Text -> Text
gtfsIdtoDomainCode gtfsId = case break (== ':') $ T.unpack gtfsId of
  (_, ':' : code) -> T.pack code
  _ -> gtfsId
