{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.FavProviderScreen.Controller where 

import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, pure, unit, bind, discard, ($), (/=), (==))
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (FavProviderScreenState)
import Components.ProviderModel as PM
import Data.Array as DA
import Helpers.Utils as HU

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> pure unit

data Action = PrimaryButtonActionController PrimaryButtonController.Action 
            | GenericHeaderAC GenericHeaderController.Action
            | BackPressed
            | FavClick String
            | ProviderModelAC PM.Action

data ScreenOutput = GoBack

eval :: Action -> FavProviderScreenState -> Eval Action ScreenOutput FavProviderScreenState

eval (PrimaryButtonActionController PrimaryButtonController.OnClick) state = do
  let _ = HU.setFavProviders state.data.selectedProviders
  exit $ GoBack

eval (FavClick id) state = continue state

eval (GenericHeaderAC (GenericHeaderController.PrefixImgOnClick )) state = continueWithCmd state [do pure BackPressed]

eval BackPressed state = exit $ GoBack

eval (ProviderModelAC (PM.FavClick config)) state = do
  let newArray =  if config.isActive then DA.delete config.id state.data.selectedProviders--DA.filter (\x -> x /= config.id) state.data.selectedProviders
                  else DA.union [config.id] state.data.selectedProviders
  continue state { data { selectedProviders = newArray }, props { buttonActive = true}}

eval _ state = continue state
