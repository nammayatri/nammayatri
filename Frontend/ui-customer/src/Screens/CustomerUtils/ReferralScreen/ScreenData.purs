module Screens.ReferralScreen.ScreenData where

import Screens.Types (ReferralScreenState)

initData :: ReferralScreenState
initData =
  { referralCode: ""
  , btnActive: false
  , showThanks: false
  , isInvalidCode: false
  , isExpandReference: false
  }
