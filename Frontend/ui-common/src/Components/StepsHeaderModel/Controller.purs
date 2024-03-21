module Components.StepsHeaderModel.Controller where

import Helpers.Utils as HU
import ConfigProvider
import Common.Types.App (LazyCheck(..)) as Lazy
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Margin(..), Padding(..))

data Action = OnArrowClick

type StepsHeaderModelState = {
  activeIndex :: Int,
  textArray :: Array String,
  backArrowVisibility :: Boolean,
  primaryBackground :: String,
  stepsTranslation :: String,
  margin :: Margin,
  padding :: Padding
}

config :: StepsHeaderModelState
config = {
    activeIndex : 0,
    textArray : [],
    backArrowVisibility : false,
    primaryBackground : "#000000",
    stepsTranslation : "Step",
    margin : MarginHorizontal 2 10,
    padding : PaddingVertical 10 10
}