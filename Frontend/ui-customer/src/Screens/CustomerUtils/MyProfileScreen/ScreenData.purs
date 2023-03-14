module Screens.MyProfileScreen.ScreenData where

import Screens.Types (MyProfileScreenState, DeleteStatus(..))

initData :: MyProfileScreenState
initData = {
    props : {
        updateProfile : false,
        accountStatus : ACTIVE
    },
    data : {
        name : "",
        mobileNumber : "",
        editedName : ""
    }
}