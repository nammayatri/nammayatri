module Screens.OnBoardingFlow.WelcomeScreen.ScreenData where

import Screens.Types (WelcomeScreenState)
import Foreign.Object (empty)
import Common.Styles.Colors as Color
import Prelude (map, (==))
import Engineering.Helpers.Commons (os)
import Common.Types.App (YoutubeData, CarouselData)

initData :: WelcomeScreenState
initData = {
  data : {
    carouselModal : 
      { gravity : "CENTER"
      , carouselData : 
          map(\item -> 
            { imageConfig : { image : item.image , height : item.imageHeight , width : 200, bgColor : "#FFFAED", cornerRadius : 8.0, isUrl : false },
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
              [ {image : "carousel_21", imageHeight : if (os == "IOS") then 260 else 380 , title : "The fastest ride booking\napp is here!", description : "Our speedy booking process means\nyou get a ride quickly and easily.", gravity : 17},
                {image : "carousel_22", imageHeight : if (os == "IOS") then 260 else 315 , title : "Dedicated\nSafety Center", description : "24X7 self serve feature and SOS for\nemergency support",  gravity : 17},
                {image : "carousel_23", imageHeight : if (os == "IOS") then 260 else 315 , title : "Inclusive and accessible,\nfor everyone!", description : "We strive to provide all our users an\neven & equal experience.", gravity : 17},
                {image : "carousel_24", imageHeight : if (os == "IOS") then 260 else 315 , title : "Be a part of the Open\nMobility Revolution!", description : "Our data and product roadmap are\ntransparent for all.", gravity : 17}
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
  , showFullScreen : false
  , hideFullScreenButton : false
  }