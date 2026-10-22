{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HotspotScreen.ComponentConfig where

import Components.PrimaryButton as PrimaryButton
import Data.String as DS
import Helpers.Utils as HU
import MerchantConfig.Types
import Language.Strings (getString)
import Language.Types
import Prelude
import PrestoDOM.Types.DomAttributes (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..), Corners(..))
import Screens.Types as ST
import Styles.Colors as Color

navigateButtonConfig :: ST.HotspotScreenState -> PrimaryButton.Config
navigateButtonConfig _ = PrimaryButton.config
  { textConfig 
    { text = getString NAVIGATE
    }
  , height = WRAP_CONTENT
  , gravity = CENTER
  , cornerRadius = 8.0
  , padding = Padding 10 14 10 15
  , margin = MarginLeft 0
  , id = "NavigateButton"
  , isPrefixImage = true
  , prefixImageConfig {
      imageUrl = "ny_ic_navigation_yellow"
    , margin = MarginRight 6
    }
  , enableRipple = true
  , rippleColor = Color.rippleShade
  }

getTextAndImage :: String -> HotspotConfig -> {text :: String, subText :: String, imageName :: String} 
getTextAndImage color hotspotConfig = do
  let circleColor = DS.toUpper color
  if DS.contains (DS.Pattern hotspotConfig.veryHighHotspotColor) circleColor
    then {text : getString VERY_HIGH_DEMAND_AREA, subText : getString THIS_AREA_IS_EXPERIENCING_VERY_HIGH_SEARCHES ,imageName : "ny_ic_very_high_hotspot"}
    else if DS.contains (DS.Pattern hotspotConfig.highHotspotColor) circleColor
          then {text : getString HIGH_DEMAND_AREA, subText : getString THIS_AREA_IS_EXPERIENCING_HIGH_SEARCHES, imageName : "ny_ic_high_hotspot"}
          else {text : getString AVERAGE_DEMAND_AREA, subText : getString THIS_AREA_IS_EXPERIENCING_AVERAGE_SEARCHES, imageName : "ny_ic_moderate_hotspot"}