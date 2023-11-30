{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ContactUsScreen.ScreenData where

import Screens.Types(ContactUsScreenState)
import Data.Maybe(Maybe(..))
import ConfigProvider
import Foreign.Object (empty)

initData :: ContactUsScreenState
initData = {
  data: {
    email : "",
    subject : "",
    description : "",
    bookingId : "",
    errorMessage : Nothing,
    config : getAppConfig appConfig,
    logField : empty
  },
  props: {
    btnActive : false,
    isSubmitted : false
  }
}
