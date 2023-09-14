module Screens.OnBoardingFlow.WelcomeScreen.ScreenData where

import Screens.Types (WelcomeScreenState, CarouselModal)
import Foreign.Object (empty)
import Common.Styles.Colors as Color

initData :: WelcomeScreenState
initData = {
  data : {
    carouselModal : [
      getCarouselData "carousel_1" 260 "#FFFAED" "The fastest auto booking\napp is here!" "Our speedy booking process means <br> you get a ride quickly and easily." 16 "#FFFAED",
      getCarouselData "carousel_2" 260 "#FFFAED" "No more\nsurge pricing!" "Experience fair and consistent fares, <br> even during peak hours." 16 "#FFFAED",
      getCarouselData "carousel_4" 260 "#FFFAED" "Inclusive and accessible \n for everyone!" "We strive to provide all our users an \n even & equal experience." 16 "#FFFAED",
      getCarouselData "carousel_3" 260 "#FFFAED" "Be a part of the Open\nMobility Revolution!" "Our data and product roadmap are\ntransparent for all." 16 "#FFFAED"
    ],
    logField : empty
  }
}


getCarouselData :: String -> Int -> String -> String -> String -> Int -> String -> CarouselModal
getCarouselData image imageHeight imageBgColor title description descTextSize carouselBgColor = {
    imageConfig : { image : image , height : imageHeight , width : 200, bgColor : imageBgColor, cornerRadius : 8.0 },
    backgroundColor : carouselBgColor,
    titleConfig : {
      text : title,
      textSize : 22,
      textColor : Color.black800,
      gravity : "CENTER",
      margin : { top : 10 , bottom : 0 , right : 20 , left : 20 }
    }, 
    descriptionConfig : {
      text : description, 
      textSize : descTextSize,
      textColor : Color.black700,
      gravity : "CENTER",
      margin : { top : 6 , bottom : 0 , right : 20 , left : 20 }
    }
}