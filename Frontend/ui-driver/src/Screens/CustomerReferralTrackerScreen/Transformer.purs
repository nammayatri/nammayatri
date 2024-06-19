{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.CustomerReferralTrackerScreen.Transformer where

import Prelude
import Data.Maybe
import Screens.Types
import Services.API as API
import Storage
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.Types as ST
import Locale.Utils
import Helpers.Utils (isToday, getCityConfig, isDateGreaterThan)
import MerchantConfig.Types (AppConfig(..))
import Data.String as STR
import LocalStorage.Cache(getValueFromCache)
import Screens.HomeScreen.Controller (getCoinPopupStatus)
import JBridge (withinTimeRange)
import Resource.Constants as RC
import Engineering.Helpers.Commons (convertUTCtoISC, getCurrentUTC)
import Screens.CustomerReferralTrackerScreen.ScreenData(CustomerReferralTrackerScreenState(..))