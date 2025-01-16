module Screens.EducationScreen.ScreenData where

import Resource.Localizable.StringsV2 as StringsV2
import Resource.Localizable.TypesV2 as LT2
import Screens.Types as ST

initData :: ST.EducationScreenState
initData = {
  videoUrl : "",
  headerText : StringsV2.getStringV2 LT2.introducing_yatri_sathi_bus_tracking,
  instructionText : StringsV2.getStringV2 LT2.introducing_yatri_sathi_bus_tracking,
  buttonText : StringsV2.getStringV2 LT2.okay,
  infoList : [ { title : StringsV2.getStringV2 LT2.introducing_yatri_sathi_bus_tracking, image : "ny_ic_blue_circle" }, { title : "Introducing Yatri Sathi bus tracking", image : "ny_ic_blue_circle" }, { title : "Introducing Yatri Sathi bus tracking", image : "ny_ic_blue_circle" }]
}