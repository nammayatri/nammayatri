module Components.StepsHeaderModel.Controller where

import Screens.Types (StepsHeaderModelState)
import Helpers.Utils as HU
import MerchantConfig.DefaultConfig as MC
import Common.Types.App (LazyCheck(..)) as Lazy

data Action = OnArrowClick

stepsHeaderData :: Int -> StepsHeaderModelState
stepsHeaderData currentIndex = {
    activeIndex : currentIndex,
    textArray : ["Let’s get you trip-ready!", "Got an OTP?", "Just one last thing"],
    backArrowVisibility : HU.showCarouselScreen Lazy.FunctionCall
  , config : MC.config
}
