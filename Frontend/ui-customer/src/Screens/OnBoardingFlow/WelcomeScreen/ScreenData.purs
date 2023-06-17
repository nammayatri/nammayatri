module Screens.OnBoardingFlow.WelcomeScreen.ScreenData where

import Screens.Types (WelcomeScreenState)

initData :: WelcomeScreenState
initData = {
  data : {
    carouselModel : [
      {image : "carousel_1", title : "The fastest auto booking app is here!", description : "Our speedy booking process means you get a ride quickly and easily."},
      {image : "carousel_2", title : "No more \n surge pricing!", description : "Experience fair and consistent fares, even during peak hours."},
      {image : "carousel_3", title : "Be a part of the Open Mobility Revolution!", description : "Our data and product roadmap are transparent for all."}
    ]
  }
}