module Screens.WelcomeScreen.ScreenData where

import Screens.Types (WelcomeScreenState)
import Foreign.Object (empty)
import Common.Styles.Colors as Color
import Prelude (map)
import Common.Types.App (YoutubeData, CarouselData)
import Language.Strings (getString)
import Language.Types(STR(..))
import Halogen.VDom.DOM.Prop (PropValue)

initData :: WelcomeScreenState
initData = {
  data : { logField : empty 
          , currentActiveIndex: 0}
}


type WelcomeScreenCarousel = (
  title :: PropValue,
  subTitle :: PropValue,
  image :: PropValue
)

type CarouselData = {
  image:: String,
   title:: String,
   description:: String,
   gravity:: Int,
   imageHeight:: Int
}