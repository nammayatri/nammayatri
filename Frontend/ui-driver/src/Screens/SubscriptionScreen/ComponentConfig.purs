{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SubscriptionScreen.ComponentConfig where

import Components.PrimaryButton as PrimaryButton
import Language.Strings
import Language.Types (STR(..))
import PrestoDOM
import Screens.Types as ST

clearDueButtonConfig :: ST.SubscriptionScreenState -> PrimaryButton.Config
clearDueButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = "Clear Dues (₹100)" }
      , isClickable = true
      , alpha = if true then 1.0 else 0.6
      , height = (V 48)
      , cornerRadius = 8.0
      , margin = (Margin 0 12 0 12)
      }
  in primaryButtonConfig'

switchPlanButtonConfig :: ST.SubscriptionScreenState -> PrimaryButton.Config
switchPlanButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = "Switch to DAILY UNLIMITED plan" }
      , isClickable = true
      , alpha = if true then 1.0 else 0.6
      , height = (V 48)
      , cornerRadius = 8.0
      , margin = (Margin 0 16 0 16)
      }
  in primaryButtonConfig'

joinPlanButtonConfig :: ST.SubscriptionScreenState -> PrimaryButton.Config
joinPlanButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = "Join Plan" }
      , isClickable = true
      , alpha = if true then 1.0 else 0.6
      , height = (V 48)
      , cornerRadius = 8.0
      , margin = (Margin 0 16 0 16)
      }
  in primaryButtonConfig'