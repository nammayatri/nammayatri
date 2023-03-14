module Screens.WriteToUsScreen.ScreenData where

import Screens.Types(WriteToUsScreenState)
import Prelude (class Eq)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep (class Generic)

initData :: WriteToUsScreenState
initData = {
  data:  { },
  props: { isThankYouScreen : false}
}


data ListOptions = Subject | YourEmaiId | DescribeYourIssue
derive instance genericListOptions :: Generic ListOptions _
instance eqListOptions :: Eq ListOptions where eq = genericEq

type Listtype =
    { value :: String,
      title :: ListOptions
    }

viewsItemList :: Array Listtype
viewsItemList = 
    [
      {title:Subject, value:"" },
      {title:YourEmaiId, value:"" },
      {title:DescribeYourIssue , value:""}
    ]