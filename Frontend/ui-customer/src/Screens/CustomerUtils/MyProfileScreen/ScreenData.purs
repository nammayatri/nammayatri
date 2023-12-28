{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.MyProfileScreen.ScreenData where

import Screens.Types (MyProfileScreenState, DeleteStatus(..))
import Data.Maybe(Maybe(..))
import ConfigProvider
import Foreign.Object (empty)

initData :: MyProfileScreenState
initData = {
    props : {
        updateProfile : false,
        genderOptionExpanded : false,
        expandEnabled : false,
        isEmailValid : true,
        isNameValid : true,
        isBtnEnabled : true,
        showOptions : false,
        fromHomeScreen : false,
        showAccessibilityPopUp : false,
        changeAccessibility : false,
        isSpecialAssistList : false
    },
    data : {
        name : "",
        mobileNumber : "",
        editedName : "",
        emailId : Nothing,
        gender : Nothing,
        editedGender : Nothing,
        editedEmailId  : Nothing,
        nameErrorMessage : Nothing,
        emailErrorMessage : Nothing,
        config : getAppConfig appConfig,
        logField : empty,
        disabilityType : Nothing,
        hasDisability : Nothing,
        editedDisabilityOptions : {
          activeIndex : 0
        , specialAssistActiveIndex : 0
        , disabilityOptionList : []
        , selectedDisability : Nothing
        , otherDisabilityReason : Nothing
        , editedDisabilityReason : ""
        },
        disabilityOptions : {
          activeIndex : 0
        , specialAssistActiveIndex : 0
        , disabilityOptionList : []
        , selectedDisability : Nothing
        , otherDisabilityReason : Nothing
        , editedDisabilityReason : ""
      }
    }
}