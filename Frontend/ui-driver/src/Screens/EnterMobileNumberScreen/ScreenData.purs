{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.EnterMobileNumberScreen.ScreenData where

import Screens.Types (EnterMobileNumberScreenState)
import Foreign.Object (empty)
import ConfigProvider
import Data.Maybe (Maybe(..))

initData :: EnterMobileNumberScreenState
initData =  
  let config = getAppConfig appConfig
  in {
  data: { mobileNumber : "",
          logField : empty,
          config,
          token : Nothing ,
          name : Nothing,
          email : Nothing,
          oauthProvider : Nothing
        },
  props: {
    btnActive : false,
    isValid : config.enterMobileNumberScreen.emailAuth,
    mobileNumberEditFocused : true,
    loginWithMobileBtnClicked : false
    }
}