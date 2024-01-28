module Screens.OnBoardingFlow.WelcomeScreen.ScreenData where

import Screens.Types (WelcomeScreenState)
import Foreign.Object (empty)
import Common.Styles.Colors as Color
import Prelude (map)
import Common.Types.App (YoutubeData, CarouselData)

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
              [ {image : "carousel_1", imageHeight : 260, title : "The fastest auto booking\napp is here!", description : "Our speedy booking process means \n you get a ride quickly and easily.", gravity : 17},
                {image : "carousel_2", imageHeight : 260, title : "No more\nsurge pricing!", description : "Experience fair and consistent fares, \n even during peak hours.",  gravity : 17 },
                {image : "carousel_4", imageHeight : 260, title : "Inclusive and accessible \n for everyone!", description : "We strive to provide all our users an \n even & equal experience.",  gravity : 17},
                {image : "carousel_3", imageHeight : 260, title : "Be a part of the Open\nMobility Revolution!", description : "Our data and product roadmap are\ntransparent for all.", gravity : 17}
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
  , showFullScreen: false
  }