module Screens.AccountSetUpScreen.ScreenData where

import Screens.Types (AccountSetUpScreenState)

initData :: AccountSetUpScreenState
initData =
  { data:
      { name: ""
      , email: ""
      }
  , props:
      { btnActive: false
      , backPressed: false
      }
  }
