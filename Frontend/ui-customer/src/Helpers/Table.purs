{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Helpers.Table where

import Data.Generic.Rep (class Generic)
import Foreign.Class (class Decode, class Encode)
import Prelude (class Eq, class Show)
import Data.Show.Generic (genericShow)
import Data.Eq.Generic (genericEq)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)
import Screens.Types (Location, Address)
import Services.API (GetProfileRes(..), SavedLocationsListRes(..))

data TableType = ProfileT
               | FlowStatusT
               | SavedLocationT

data TableData = ProfileD GetProfileRes
               | FlowStatusD FlowStatusData
               | SavedLocationD SavedLocationsListRes

derive instance genericTableData :: Generic TableData _
instance showTableData :: Show TableData where show = genericShow
instance encodeTableData :: Encode TableData where encode = defaultEncode
instance decodeTableData :: Decode TableData where decode = defaultDecode

newtype FlowStatusData = FlowStatusData {
    source :: Location
  , destination :: Location
  , sourceAddress :: Address
  , destinationAddress :: Address
}

derive instance genericFlowStatusData :: Generic FlowStatusData _
instance showFlowStatusData :: Show FlowStatusData where show = genericShow
instance encodeFlowStatusData :: Encode FlowStatusData where encode = defaultEncode
instance decodeFlowStatusData :: Decode FlowStatusData where decode = defaultDecode




