{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Screens.HomeScreen.Transformer
  ( accessbilityBannerConfig
  , genderBannerConfig
  , getBannerConfigs
  , getDisabledLocById
  )
  where

import Prelude
import Screens.HomeScreen.ComponentConfig
import Screens.Types

import Components.PSBanner as PSBanner
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.HomeScreen.Controller (Action(..))
import Styles.Colors as Color

getDisabledLocById :: String -> Array GoToLocation -> Array GoToLocation
getDisabledLocById id gotoArray = (map (\item ->  {   
    id : item.id,
    lat : item.lat,
    lon : item.lon,
    address : item.address,
    tag : item.tag,
    disabled : item.id == id || item.disabled
    }) gotoArray)


getBannerConfigs :: HomeScreenState -> Array (PSBanner.Config (PSBanner.Action -> Action))
getBannerConfigs state = [
  genderBannerConfig state
, accessbilityBannerConfig state
] 


genderBannerConfig :: forall a. HomeScreenState -> PSBanner.Config  (PSBanner.Action -> Action)
genderBannerConfig state =
  let
    config = PSBanner.config $ BannerCarousal
    config' = config
      {
        backgroundColor = Color.green600,
        title = (getString COMPLETE_YOUR_PROFILE_AND_FIND_MORE_RIDES),
        titleColor = Color.white900,
        actionText = (getString UPDATE_NOW),
        actionTextColor = Color.white900,
        imageUrl = "ny_ic_driver_gender_banner",
        isBanner = true
      }
  in config'


accessbilityBannerConfig :: HomeScreenState -> PSBanner.Config (PSBanner.Action -> Action)
accessbilityBannerConfig state = 
  let 
    config = PSBanner.config BannerCarousal
    config' = config  
      {
        backgroundColor = Color.lightPurple,
        title = getString LEARN_HOW_YOU_CAN_HELP_CUSTOMERS_REQUIRING_SPECIAL_ASSISTANCE,
        titleColor = Color.purple,
        actionText = getString LEARN_MORE,
        actionTextColor = Color.purple,
        imageUrl = "ny_ic_purple_badge",
        isBanner = true
      }
  in config'

-- paymentStatusConfig :: HomeScreenState -> PSBanner.Config Action
-- paymentStatusConfig state = 
--   let 
--     config = PSBanner.config BannerCarousal
--     confign = config
--       { 
--         backgroundColor = state.data.paymentState.bannerBG,
--         title = state.data.paymentState.bannerTitle,
--         titleColor = state.data.paymentState.bannerTitleColor,
--         actionText = state.data.paymentState.banneActionText,
--         actionTextColor = state.data.paymentState.actionTextColor,
--         imageUrl = state.data.paymentState.bannerImage,
--         isBanner = true
--       }
--   in confign