module Screens.OnBoardingFlow.WelcomeScreen.ScreenData where

import Screens.Types (WelcomeScreenState)
import Foreign.Object (empty)

initData :: WelcomeScreenState
initData = {
  data : {
    carouselModel : [
      {image : "carousel_1", title : "The fastest auto booking\napp is here!", description : "Our speedy booking process means\nyou get a ride quickly and easily.", 
       imageConfig : {
        height : 300 , width : 300
       },
       titleConfig : {
        textColor : "#454545",
        textSize : 22 
       } ,
       descriptionConfig : {
        textColor : "#6D7280",
        textSize : 16 
       }},
      {image : "carousel_2", title : "No more\nsurge pricing!", description : "Experience fair and consistent fares,\neven during peak hours." , 
      imageConfig : {
        height : 300 , width : 300
       },
       titleConfig : {
        textColor : "#454545",
        textSize : 22 
       } ,
       descriptionConfig : {
        textColor : "#6D7280",
        textSize : 16 
       }},
      {image : "carousel_4", title : "Inclusive and accessible for everyone!", description : "We strive to provide all our users an \n even & equal experience.",
      imageConfig : {
        height : 300 , width : 300
       },
       titleConfig : {
        textColor : "#454545",
        textSize : 22 
       },
       descriptionConfig : {
        textColor : "#6D7280",
        textSize : 16 
       }},
      {image : "carousel_3", title : "Be a part of the Open\nMobility Revolution!", description : "Our data and product roadmap are\ntransparent for all.",
      imageConfig : {
        height : 300 , width : 300
       },
       titleConfig : {
        textColor : "#454545",
        textSize : 22 
       },
       descriptionConfig : {
        textColor : "#6D7280",
        textSize : 16
       }}
    ],
    logField : empty
  }
}

        -- this.imageHeight = imageHeight;
        -- this.imageWidth = imageWidth;
        -- this.subheadingColor = subheadingColor;
        -- this.subHeadingMarginTop = subHeadingMarginTop;
        -- this.headingColor = headingColor;
        -- this.headingMarginTop = headingMarginTop;