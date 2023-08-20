#!/bin/bash
touch "$3/Handler.purs"

lowerCamelCase=$(echo "${1:0:1}" | tr '[:upper:]' '[:lower:]')${1:1}
upperSnakeCase=$(echo "$lowerCamelCase" | sed -e 's/\([a-z]\)\([A-Z]\)/\1_\2/g' -e 's/\([a-zA-Z]\+\)/\U&/g')
capitalized=$(echo "$upperSnakeCase" | tr '[:lower:]' '[:upper:]')

if [ "$2" == "ui-driver" ]; then 
  importStatement="import Types.ModifyScreenState (modifyScreenState)"
else
  importStatement="import ModifyScreenState (modifyScreenState)"
fi

echo "{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.$1Screen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, discard, ($), (<$>))
import Screens.$1Screen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.$1Screen.View as $1Screen
import Types.App (FlowBT, GlobalState(..), ${capitalized}_SCREEN_OUTPUT(..),ScreenType(..))
$importStatement

${lowerCamelCase}Screen :: FlowBT String ${capitalized}_SCREEN_OUTPUT
${lowerCamelCase}Screen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ $1Screen.screen state.${lowerCamelCase}Screen
  case action of
    _ -> App.BackT $ pure App.GoBack " > "$3/Handler.purs"

echo "Handler.purs generated Successfully ! ------------------------------------------- ✔"