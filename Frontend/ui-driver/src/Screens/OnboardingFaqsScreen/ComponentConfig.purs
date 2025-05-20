{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.OnboardingFaqsScreen.ComponentConfig where

import Mobility.Prelude

import Common.Types.App (LazyCheck(..))
import Components.Calendar.Controller as CalendarConfig
import Components.ErrorModal as ErrorModal
import Components.ErrorModal.Controller (Action(..), Config)
import Components.GenericHeader.Controller as GenericHeaderConfig
import Components.PopUpModal as PopUpModalConfig
import Components.PrimaryButton.Controller as PrimaryButtonConfig
import Components.PrimaryButton.View as PrimaryButton
import Components.RequestInfoCard as RequestInfoCard
import Data.Int (toNumber)
import Data.Maybe (isJust, fromMaybe)
import Engineering.Helpers.Utils (getCurrentDay)
import Engineering.Helpers.Utils (getFixedTwoDecimals)
import Font.Style (Style(..))
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import LocalStorage.Cache (getValueFromCache)
import Prelude ((<>), (==), (*), show, not, (&&), ($))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), background)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types as ST
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color
import Data.Array as DA
import Font.Style as FontStyle

genericHeaderConfig :: ST.OnboardingFaqsScreenState -> GenericHeaderConfig.Config
genericHeaderConfig state =
    let headerText = 
            if state.props.showAns 
            then "" 
            else if isJust state.props.selectedCategory then fromMaybe "" state.props.selectedCategory
            else "FAQs"
    in
    GenericHeaderConfig.config
      { height = WRAP_CONTENT
      , textConfig
        { text = headerText
        , color = Color.darkCharcoal
        }
      , prefixImageConfig
        { height = V 30
        , width = V 30
        , imageUrl = "ny_ic_chevron_left_black"
        , enableRipple = true
        , margin = Margin 8 8 8 8
        , layoutMargin = Margin 4 4 4 4
        }
      , suffixImageConfig
        { visibility = GONE
        }
      }