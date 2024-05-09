{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideConfirmationScreen.View where 

screen :: ST.RideConfirmationScreenState -> Screen Action ST.RideConfirmationScreenState ScreenOutput
screen initialState = 
  { initialState 
  , view 
  , name : "RideConfirmationScreen"
  , globalEvents : [] 
  , eval : 
     \action state -> do 
        let _ = spy "RideConfirmationScreen action " action
        let _ = spy "RideConfirmationScreen state " state
        eval action state
  }

view :: forall w . (Action -> Effect Unit) -> ST.SelectLanguageScreenState -> PrestoDOM (Effect Unit) w
view push state = 
  PrestoAnim.animationSet [Anim.fadeIn true] $
  linearLayout