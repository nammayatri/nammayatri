module Components.BottomDrawerList.Controller where

import Screens.Types as ST

data Action = OnItemClick ListComponent
            | Dismiss
            | OnAnimationEnd

type Config = {
    itemList :: Array ListComponent,
    animState :: ST.AnimType,
    titleText :: String
}

type ListComponent = {
  prefixImg :: String,
  title :: String,
  desc :: String,
  postFixImg :: String,
  visibility :: Boolean,
  identifier :: String
}

config :: Config
config = {
    itemList : [],
    animState : ST.HIDE,
    titleText : ""
}