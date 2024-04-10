{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.AskPermissionScreen.ComponentConfig where

import Components.PrimaryButton as PrimaryButton
import PrestoDOM (Margin(..))
import Screens.Types as ST
import Styles.Colors as Color
import Prelude ((==), ($))
import Language.Strings (getString)
import Language.Types (STR(..))
import Components.StepsHeaderModel as SHM
import Data.Array as DA
import Data.Maybe (fromMaybe, maybe)

primaryButtonConfig :: ST.AskPermissionScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config

    primaryButtonConfig' =
      config
        { textConfig
          { text = getString ALLOW
          }
        , margin = Margin 16 16 16 16
        , id = "AllowButton"
        }
  in
    primaryButtonConfig'

stepsHeaderData :: ST.AskPermissionScreenState -> SHM.StepsHeaderModelState
stepsHeaderData state =
  let
    arr = DA.replicate (DA.length state.data.permissionList) (getString GRANT_PERMISSIONS)

    currentIndex = maybe 0 (\currentItem -> fromMaybe 0 $ DA.findIndex (\item -> item == currentItem) state.data.permissionList) state.props.currentStep
  in
    SHM.config
      { activeIndex = currentIndex
      , textArray = arr
      , backArrowVisibility = state.props.backpressEnabled
      , primaryBackground = Color.black900
      , stepsTranslation = getString STEP
      }
