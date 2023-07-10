{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Storage.Redis.Types where

import Data.String.Conversions
import Data.Text
import Prelude

type MBInteger = Maybe Integer

newtype Count = Count MBInteger

newtype BlockTime = BlockTime MBInteger

type GroupName = Text

type ConsumerName = Text

data Consumer = Consumer GroupName ConsumerName

data EntryId = AutoId | EntryId Text Text | NewId -- not all types of ids are included here.

instance Show EntryId where
  show AutoId = "*"
  show (EntryId ts ind) = cs $ ts <> "-" <> ind
  show NewId = ">"
