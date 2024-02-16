{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverReferralScreen.ComponentConfig where

import Common.Types.App
import Data.Maybe
import Data.String
import Helpers.Utils
import Language.Strings
import Prelude
import PrestoDOM

import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText.Views as PrimaryEditText
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Types (STR(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.ReferralScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color

--------------------------------------------------- genericHeaderConfig -----------------------------------------------------
genericHeaderConfig :: ST.DriverReferralScreenState -> GenericHeader.Config
genericHeaderConfig state = let
  cityConfig = getCityConfig state.data.config.cityConfig (getValueToLocalStore DRIVER_LOCATION)
  bothReferralNotEnabled = not (cityConfig.showDriverReferral || state.data.config.enableDriverReferral || cityConfig.showCustomerReferral || state.data.config.enableCustomerReferral)
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
        text = case bothReferralNotEnabled of
          true -> getString RIDE_LEADERBOARD
          false -> getString REFERRAL 
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
      , height = V 48
      , width = MATCH_PARENT
      , id = "DriverReferralScreenPrimaryButton"
      }
  in primaryButtonConfig'