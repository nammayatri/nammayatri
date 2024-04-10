module Components.StepsHeaderModel.Controller where

import Helpers.Utils as HU
import ConfigProvider
import Common.Types.App (LazyCheck(..)) as Lazy
import Language.Strings (getString)
import Language.Types (STR(..))

data Action = OnArrowClick

type StepsHeaderModelState = {
  activeIndex :: Int,
  textArray :: Array String,
  backArrowVisibility :: Boolean,
  primaryBackground :: String,
  stepsTranslation :: String
}

config :: StepsHeaderModelState
config = {
    activeIndex : 0,
    textArray : [],
    backArrowVisibility : false,
    primaryBackground : "#000000",
    stepsTranslation : "Step"
}