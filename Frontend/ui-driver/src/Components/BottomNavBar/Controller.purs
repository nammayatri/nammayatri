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
import Helpers.Utils (fetchImage, FetchImageFrom(..))
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
          activeIcon: fetchImage FF_ASSET "ny_ic_home_active",
          defaultIcon: fetchImage FF_ASSET "ny_ic_home_inactive",
          text: "Home",
          showNewBanner : bottomNavConfig.home.showNew,
          isVisible : bottomNavConfig.home.isVisible,
          screenName : ScreenNames.HOME_SCREEN
        },
        {
          activeIcon: fetchImage FF_ASSET "ny_ic_rides_tab_active",
          defaultIcon: fetchImage FF_ASSET "ny_ic_rides_tab_inactive",
          isVisible : bottomNavConfig.rideHistory.isVisible,
          showNewBanner : bottomNavConfig.rideHistory.showNew,
          text: "Rides",
          screenName : ScreenNames.RIDE_HISTORY_SCREEN
        },
        {
          activeIcon: fetchImage FF_ASSET "ny_ic_join_active",
          defaultIcon: fetchImage FF_ASSET "ny_ic_join_inactive",
          isVisible : bottomNavConfig.subscription.isVisible,
          showNewBanner : bottomNavConfig.subscription.showNew && showNewBannerOnSubscription ,
          text: "Join",
          screenName : ScreenNames.SUBSCRIPTION_SCREEN
        },
        {
          activeIcon: fetchImage FF_ASSET "ic_referral_active",
          defaultIcon: fetchImage FF_ASSET $ if (getValueToLocalNativeStore REFERRAL_ACTIVATED) == "true" then  "ny_ic_contest_alert" else "ic_referral_inactive",
          isVisible : bottomNavConfig.referral.isVisible,
          showNewBanner : bottomNavConfig.referral.showNew,
          text: "Rankings",
          screenName : ScreenNames.REFERRAL_SCREEN
        },
        {
          activeIcon: fetchImage FF_ASSET "ny_ic_alerts_active",
          defaultIcon: fetchImage FF_ASSET "ny_ic_alerts_inactive",
          text: "Alert",
          isVisible : bottomNavConfig.notifications.isVisible,
          showNewBanner : bottomNavConfig.notifications.showNew,
          screenName : ScreenNames.ALERTS_SCREEN
        }
      ]
      processedNavOptions = DA.filter (_.isVisible) navdata
  {
   activeIndex : getActiveIndex screenName processedNavOptions,
   navButton: navdata
  }

getActiveIndex :: ScreenNames.ScreenName -> Array NavIcons -> Int
getActiveIndex screenName navstate = Maybe.fromMaybe 0 $ DA.findIndex (\item -> item.screenName == screenName) navstate