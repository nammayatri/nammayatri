{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Screens.DriverReferralScreen.ScreenData where


import Screens.Types (DriverReferralScreenState, ReferredUserType(..), UserReferralType(..))
import Foreign.Object (empty)
import ConfigProvider
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Screens.RegistrationScreen.ScreenData (dummyCityConfig)

initData :: DriverReferralScreenState
initData = {
    data : { logField : empty 
         , config : getAppConfig appConfig
         , referredDrivers : "--"
         , referralCode : ""
         , referredCustomers : ""
         , activatedCustomers : ""
         , cityConfig : dummyCityConfig
        },
    props : {
      showDriverReferralQRCode : false,
      showNewDriverReferralText : true,
      referralInfo : NO_POPUP,
      currentReferralItem : Just CUSTOMER_REFERRAL
    }
}
