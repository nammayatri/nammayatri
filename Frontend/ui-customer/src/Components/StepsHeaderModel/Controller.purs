module Components.StepsHeaderModel.Controller where

import Screens.Types (StepsHeaderModelState)
import Helpers.Utils as HU
import ConfigProvider
import Constants
import Common.Types.App (LazyCheck(..)) as Lazy
import Language.Strings (getString)
import Language.Types (STR(..))

data Action = OnArrowClick

stepsHeaderData :: Int -> StepsHeaderModelState
stepsHeaderData currentIndex = {
    activeIndex : currentIndex,
    textArray : [(getString LETS_GET_YOU_TRIP_READY), (getString GOT_AN_OTP), (getString JUST_ONE_LAST_THING)],
    backArrowVisibility : HU.showCarouselScreen Lazy.FunctionCall
  , config : getAppConfig appConfig
}
