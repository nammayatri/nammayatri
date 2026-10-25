{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.CheckListView.Controller where

import Common.Types.App (CheckBoxOptions)
import Prelude (class Show)

data Action = NoAction | ChangeCheckBoxSate CheckBoxOptions

instance showAction :: Show Action where
  show (NoAction) = "NoAction"
  show (ChangeCheckBoxSate _) = "ChangeCheckBoxSate"

type Config = { 
      optionsProvided :: Array CheckBoxOptions, 
      isSelected :: Boolean ,
      index :: Int
      }

config :: Config
config = {
    optionsProvided : [],
    isSelected : false,
    index : 0
  }
