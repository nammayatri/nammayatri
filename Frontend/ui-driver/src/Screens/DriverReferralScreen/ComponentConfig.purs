{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverReferralScreen.ComponentConfig where

import Components.GenericHeader as GenericHeader
import Components.PrimaryEditText.Views as PrimaryEditText
import Components.PrimaryButton as PrimaryButton
import Components.PopUpModal as PopUpModal
import Screens.ReferralScreen.Controller (Action(..), ScreenOutput, eval)
import Effect (Effect)
import Common.Types.App
import Data.Maybe
import Data.String
import Font.Style as FontStyle
import PrestoDOM.Types.DomAttributes (Corners(..))
import Font.Size as FontSize
import Language.Strings
import Language.Types (STR(..))
import Prelude
import PrestoDOM
import Screens.Types as ST
import Styles.Colors as Color


--------------------------------------------------- genericHeaderConfig -----------------------------------------------------
genericHeaderConfig :: ST.DriverReferralScreenState -> GenericHeader.Config
genericHeaderConfig state = let
  config = GenericHeader.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , width = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = "ny_ic_chevron_left"
      , margin = Margin 12 12 12 12
      , visibility = GONE
      }
    , padding = Padding 16 16 0 16
    , textConfig {
        text = getString RANKINGS
      , color = Color.darkCharcoal
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'

primaryButtonConfig :: ST.DriverReferralScreenState -> PrimaryButton.Config 
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig
      { text = getString GO_BACK
      , color = state.data.config.primaryTextColor
      }
      , margin = Margin 10 10 10 10
      , background = Color.black900
      , height = V 60
      , id = "DriverReferralScreenPrimaryButton"
      }
  in primaryButtonConfig'