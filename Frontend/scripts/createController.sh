#!/bin/bash
touch "$3/Controller.purs"

echo "{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.$1Screen.Controller where 

import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Prelude (class Show, pure, unit, bind, discard, ($), (/=), (==))
import PrestoDOM (Eval, continue, continueWithCmd, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types ($1ScreenState)

instance showAction :: Show Action where
  show _ = \"\"

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> pure unit

data Action = PrimaryButtonActionController PrimaryButtonController.Action 
            | GenericHeaderAC GenericHeaderController.Action
            | BackPressed

data ScreenOutput = GoBack

eval :: Action -> $1ScreenState -> Eval Action ScreenOutput $1ScreenState

eval (PrimaryButtonActionController PrimaryButtonController.OnClick) state = continue state

eval (GenericHeaderAC (GenericHeaderController.PrefixImgOnClick )) state = continueWithCmd state [do pure BackPressed]

eval BackPressed state = exit $ GoBack

eval _ state = continue state" > "$3/Controller.purs"

echo "Controller.purs generated Successfully ! ------------------------------------------- ✔"