module Screens.EditAadhaarDetailsScreen.ScreenData where

import Screens.Types(EditAadhaarDetailsScreenState)
import Prelude (class Eq)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep (class Generic)


initData :: EditAadhaarDetailsScreenState
initData = {
  data:  {},
  props: {
    isInEditAadharDetailsScreen : false
  }
}

data ListOptions = AADHAAR_NUMBER | IMAGE_FRONT_SIDE | IMAGE_BACK_SIDE
derive instance genericListOptions :: Generic ListOptions _
instance eqListOptions :: Eq ListOptions where eq = genericEq

type Listtype =
    { value :: String,
      title :: ListOptions
    }

viewsItemList :: Array Listtype
viewsItemList = 
    [
      {title:AADHAAR_NUMBER, value:"" },
      {title:IMAGE_FRONT_SIDE, value:"image.png" },
      {title:IMAGE_BACK_SIDE, value:"image.png" }
    ]