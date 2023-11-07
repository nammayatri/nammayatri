module Screens.WelcomeScreen.ScreenData where

import Screens.Types (WelcomeScreenState)
import Foreign.Object (empty)
import Common.Styles.Colors as Color
import Prelude (map)
import Common.Types.App (YoutubeData, CarouselData)
import Language.Strings (getString)
import Language.Types(STR(..))

initData :: WelcomeScreenState
initData = {
  data : {
    carouselModal : 
      { gravity : "CENTER"
      , carouselData : 
          map(\item -> 
            { imageConfig : { image : item.image , height : item.imageHeight , width : 200, bgColor : "#FFFAED", cornerRadius : 8.0 },
              backgroundColor : "#FFFAED",
              youtubeConfig : dummyYoutubeData,
              gravity : item.gravity ,
              contentType : "IMAGE" , 
              titleConfig : {
                text : item.title,
                textSize : 22,
                textColor : Color.black800,
                gravity : "CENTER",
                margin : { top : 10 , bottom : 0 , right : 20 , left : 20 }
              }, 
              descriptionConfig : {
                text : item.description, 
                textSize : 16,
                textColor : Color.black700,
                gravity : "CENTER",
                margin : { top : 6 , bottom : 0 , right : 20 , left : 20 }
              }
            })
              [ {image : "ny_ic_welcome_screen_1", title : getString DIRECT_PAYMENT_NO_COMMISSIONS , description : getString CUSTOMER_PAYS_DIRECTLY, gravity : 17, imageHeight : 260},
                {image : "ny_ic_welcome_screen_2", title : getString HUNDRED_PERCENT_FARE_GOES_TO_YOU, description : getString FARE_SHOWN_IS_FARE_YOU_GET, gravity : 17, imageHeight : 260},
                {image : "ny_ic_welcome_screen_3", title : getString BE_A_PART_OF_OPEN_MOBILITY_REVOLUTION, description : getString CUSTOMER_PAYS_DIRECTLY, gravity : 17, imageHeight : 260}
              ]
    },
    logField : empty
  }
}


dummyYoutubeData :: YoutubeData
dummyYoutubeData = { videoTitle: ""
  , setVideoTitle: false
  , showMenuButton: false
  , showDuration: true
  , showSeekBar: true
  , videoId: ""
  , videoType: ""
  , videoHeight : 0
  }