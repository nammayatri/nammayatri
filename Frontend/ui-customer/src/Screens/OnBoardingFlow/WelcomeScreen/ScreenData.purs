module Screens.OnBoardingFlow.WelcomeScreen.ScreenData where

import Screens.Types (WelcomeScreenState, CarouselModal)
import Foreign.Object (empty)
import Common.Styles.Colors as Color

initData :: WelcomeScreenState
initData = {
  data : {
    carouselModal : [
      getCarouselData "carousel_1" 300 "#FFFAED" "The fastest auto booking\napp is here!" "Our speedy booking process means <br> you get a ride quickly and easily." 16,
      getCarouselData "carousel_2" 300 "#FFFAED" "No more\nsurge pricing!" "Experience fair and consistent fares, <br> even during peak hours." 16,
      getCarouselData "carousel_4" 300 "#FFFAED" "Inclusive and accessible for everyone!" "We strive to provide all our users an \n even & equal experience." 16,
      getCarouselData "carousel_3" 300 "#FFFAED" "Be a part of the Open\nMobility Revolution!" "Our data and product roadmap are\ntransparent for all." 16
    ],
    logField : empty
  }
}


getCarouselData :: String -> Int -> String -> String -> String -> Int -> CarouselModal
getCarouselData image imageHeight imageBgColor title description descTextSize = {
    imageConfig : { image : image , height : imageHeight , width : 200, bgColor : imageBgColor, cornerRadius : 8.0 },
    titleConfig : {
      text : title,
      textSize : 22,
      textColor : Color.black800,
      gravity : "CENTER",
      margin : { top : 48 , bottom : 32 , right : 80 , left : 80 }
    }, 
    descriptionConfig : {
      text : description, 
      textSize : descTextSize,
      textColor : Color.black700,
      gravity : "CENTER",
      margin : { top : 0 , bottom : 32 , right : 80 , left : 80 }
    }
}