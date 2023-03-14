module Screens.SelectLanguageScreen.ScreenData where

import Screens.Types(SelectLanguageScreenState)

initData :: SelectLanguageScreenState
initData = {
    data: {
      languages : 
        [
          {name:"English",value:"EN_US", subtitle: ""}, 
          {name:"ಕನ್ನಡ",value:"KN_IN", subtitle: "Kannada"},
          {name:"हिंदी",value:"HI_IN", subtitle: "Hindi"},
          {name:"தமிழ்",value:"TA_IN", subtitle: "Tamil"}
        ]
      , isSelected : true
    },
    props: {
      btnActive :false,
      selectedLanguage: ""
    }
}