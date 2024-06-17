module Screens.BankDetailsScreen.ScreenData where

import Prelude


initData :: BankDetailsScreenState
initData = {
  data : {
    dummyData : false
  },
  props : {
    dummyProps : false
  }
}

type BankDetailsScreenState = {
  data :: {
    dummyData :: Boolean
  },
  props :: {
    dummyProps :: Boolean
  }
}