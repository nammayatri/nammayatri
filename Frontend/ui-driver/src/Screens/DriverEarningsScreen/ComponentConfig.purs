{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverEarningsScreen.ComponentConfig where


import Common.Types.App (LazyCheck(..))
import Language.Strings (getString)
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Components.ErrorModal as ErrorModal
import Components.GenericHeader.Controller as GenericHeaderConfig
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Types (STR(..))
import Screens.Types as ST
import Styles.Colors as Color
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink, getPastDays)
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))
import Components.DatePickerModel as DatePickerModel
import Resource.Constants (tripDatesCount)
import Components.PrimaryButton.View as PrimaryButton
import Components.PrimaryButton.Controller as PrimaryButtonConfig
import Components.ErrorModal.Controller (Action(..), Config)

primaryButtonConfig :: Boolean -> Boolean -> PrimaryButtonConfig.Config
primaryButtonConfig isActive isVisible = let
    config' = PrimaryButtonConfig.config
    primaryButtonConfig' = config'
      { textConfig
        { text = "Buy Now"
        , accessibilityHint = (" : Buy Now")
        }
      , cornerRadius = 6.0
      , visibility = if isVisible then VISIBLE else GONE
      , margin = Margin 16 24 16 8
      , isClickable = isActive
      , alpha = if isActive then 1.0 else 0.5
      }
  in primaryButtonConfig'

genericHeaderConfig :: ST.DriverEarningsScreenState -> GenericHeaderConfig.Config
genericHeaderConfig state = let
  config = GenericHeaderConfig.config
  genericHeaderConfig' = config 
    { height = WRAP_CONTENT
    , padding = (PaddingVertical 5 5)
    , textConfig {
        text = "Use Coins"
      , color = Color.darkCharcoal }
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = "ny_ic_chevron_left," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_left.png"
      } 
    , suffixImageConfig {
        visibility = GONE }
    }
  in genericHeaderConfig'