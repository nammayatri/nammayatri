{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.BottomNavBar.Controller where

import Common.Types.App (LazyCheck(..))
import Data.Array as DA
import Data.Int (fromString)
import Data.Maybe as Maybe
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import MerchantConfig.Types (BottomNavConfig)
import MerchantConfig.Utils (Merchant(..), getMerchant, getValueFromConfig)
import Prelude (bottom, ($), (<>), (||), unit, (<>), (==), negate, (/=), (<), (&&))
import Screens as ScreenNames
import Screens.Types (BottomNavBarState, NavIcons)
import Storage (KeyStore(..), getValueToLocalNativeStore)

data Action = OnNavigate String

navData :: ScreenNames.ScreenName -> BottomNavConfig -> BottomNavBarState
navData screenName bottomNavConfig = do
  let showNewBannerOnSubscription = (Maybe.fromMaybe 0 $ fromString $ getValueToLocalNativeStore TIMES_OPENED_NEW_SUBSCRIPTION) < 3
      navdata = [
        {
          activeIcon: "ny_ic_home_active," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_home_active.png",
          defaultIcon: "ny_ic_home_inactive," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_home_inactive.png",
          text: "Home",
          showNewBanner : bottomNavConfig.home.showNew,
          isVisible : bottomNavConfig.home.isVisible,
          screenName : ScreenNames.HOME_SCREEN
        },
        {
          activeIcon: if (getMerchant FunctionCall == NAMMAYATRI) then "ny_ic_rides_active," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_rides_active.png" else "ny_ic_cab_active,https://assets.juspay.in/beckn/merchantcommon/images/ny_ic_cab_active.png",
          defaultIcon: if (getMerchant FunctionCall == NAMMAYATRI) then "ny_ic_rides_inactive," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_rides_inactive.png" else "ny_ic_cab_inactive,https://assets.juspay.in/beckn/merchantcommon/images/ny_ic_cab_inactive.png",
          isVisible : bottomNavConfig.rideHistory.isVisible,
          showNewBanner : bottomNavConfig.rideHistory.showNew,
          text: "Rides",
          screenName : ScreenNames.RIDE_HISTORY_SCREEN
        },
        {
          activeIcon: "ny_ic_join_active," <> (getAssetStoreLink FunctionCall) <> "ny_ic_join_active.png",
          defaultIcon: "ny_ic_join_inactive," <> (getAssetStoreLink FunctionCall) <> "ny_ic_join_inactive.png",
          isVisible : bottomNavConfig.subscription.isVisible,
          showNewBanner : bottomNavConfig.subscription.showNew && showNewBannerOnSubscription ,
          text: "Join",
          screenName : ScreenNames.SUBSCRIPTION_SCREEN
        },
        {
          activeIcon: "ic_referral_active," <> (getAssetStoreLink FunctionCall) <> "ic_referral_active.png",
          defaultIcon: if (getValueToLocalNativeStore REFERRAL_ACTIVATED) == "true" then  "ny_ic_contest_alert," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_contest_alert.png" else "ic_referral_inactive," <> (getCommonAssetStoreLink FunctionCall) <> "ic_referral_inactive.png",
          isVisible : bottomNavConfig.referral.isVisible,
          showNewBanner : bottomNavConfig.referral.showNew,
          text: "Rankings",
          screenName : ScreenNames.REFERRAL_SCREEN
        },
        {
          activeIcon: "ny_ic_alerts_active",
          defaultIcon: "ny_ic_alerts_inactive," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_alerts_inactive.png",
          text: "Alert",
          isVisible : bottomNavConfig.notifications.isVisible,
          showNewBanner : bottomNavConfig.notifications.showNew,
          screenName : ScreenNames.ALERTS_SCREEN
        }
      ]
      processedNavOptions = DA.filter (\item -> item.isVisible == true) navdata
  {
   activeIndex : getActiveIndex screenName processedNavOptions,
   navButton: navdata
  }

getActiveIndex :: ScreenNames.ScreenName -> Array NavIcons -> Int
getActiveIndex screenName navstate = Maybe.fromMaybe 0 $ DA.findIndex (\item -> item.screenName == screenName) navstate