module Screens.ChooseLanguageScreen.ScreenData where

import Screens.Types (ChooseLanguageScreenState)

initData :: ChooseLanguageScreenState
initData = 
  { data: 
      {
        languages : [
        { name : "English"
        , value : "EN_US"
        , subTitle : ""
        },
        { name : "ಕನ್ನಡ"
        , value : "KN_IN"
        , subTitle : "Kannada"
        },
        { name : "हिंदी"
        , value : "HI_IN"
        , subTitle : "Hindi"
        }]
        , isSelected : false
       }
    ,props:
      {
          selectedLanguage : "EN_US"
        , btnActive : true
        , exitAnimation : false
      }
    }