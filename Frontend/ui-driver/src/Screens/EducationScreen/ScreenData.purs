module Screens.EducationScreen.ScreenData where

import Resource.Localizable.StringsV2 as StringsV2
import Resource.Localizable.TypesV2 as LT2
import Screens.Types as ST

initData :: ST.EducationScreenState
initData = {
  videoUrl : "",
  headerText : "",
  instructionText : "",
  buttonText : "",
  descriptionList : []
}