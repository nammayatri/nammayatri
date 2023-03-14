module Screens.EditBankDetailsScreen.ScreenData where

import Screens.Types(EditBankDetailsScreenState)
import Prelude (class Eq)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep (class Generic)


initData :: EditBankDetailsScreenState
initData = {
  data:  {

  },

  props: {
    isInEditBankDetailsScreen : false
  }
}

data ListOptions = DRIVER_BANK_NAME | ACCOUNT_NO | IFSC
derive instance genericListOptions :: Generic ListOptions _
instance eqListOptions :: Eq ListOptions where eq = genericEq

type Listtype =
    { value :: String,
      title :: ListOptions
    }

viewsItemList :: Array Listtype
viewsItemList = 
    [
      {title:DRIVER_BANK_NAME, value:"" },
      {title:ACCOUNT_NO, value:"" },
      {title:IFSC, value:"" }
    ]