module Components.StepsHeaderModel.Controller where

import Screens.Types (StepsHeaderModelState)
import Merchant.Utils as MU
import Common.Types.App (LazyCheck(..)) as Lazy

data Action = OnArrowClick

stepsHeaderData :: Int -> StepsHeaderModelState
stepsHeaderData currentIndex = {
    activeIndex : currentIndex,
    textArray : ["Let’s get you trip-ready!", "Got an OTP?", "Just one last thing"],
    backArrowVisibility : MU.showCarouselScreen Lazy.FunctionCall
}
