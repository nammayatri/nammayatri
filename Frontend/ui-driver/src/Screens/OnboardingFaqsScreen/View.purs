{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.OnboardingFaqsScreen.View where

import Animation as Anim
import Data.Maybe
import Debug
import Effect (Effect)
import Prelude
import PrestoDOM
import PrestoDOM.Types.Core
import Screens.Types as ST
import Screens.OnboardingFaqsScreen.Controller (Action(..), ScreenOutput, eval)
import Components.PrimaryButton as PrimaryButton
import Engineering.Helpers.Commons as EHC
import Styles.Colors as Color
import Font.Style as FontStyle
import Screens.OnboardingFaqsScreen.ScreenData
import Resource.Localizable.StringsV2
import Mobility.Prelude
import Resource.Localizable.TypesV2 as LT2

screen :: ST.OnboardingFaqsScreenState -> Screen Action ST.OnboardingFaqsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "OnboardingFaqsScreen"
  , globalEvents: []
  , eval:
      \action state -> do
        let
          _ = spy "OnboardingFaqsScreen ----- state" state
        let
          _ = spy "OnboardingFaqsScreen --------action" action
        eval action state
  }

view :: forall w. (Action -> Effect Unit) -> ST.OnboardingFaqsScreenState -> PrestoDOM (Effect Unit) w
view push state = 
    linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.white900
    , onBackPressed push $ const BackPressed
    ][]