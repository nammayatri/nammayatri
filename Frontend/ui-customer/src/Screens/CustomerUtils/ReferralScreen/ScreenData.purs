{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ReferralScreen.ScreenData where

import ConfigProvider
import Screens.Types (ReferralScreenState, ReferralType(..), ReferralStage(..))
import Foreign.Object (empty)
import Data.Maybe (Maybe(..))

initData :: ReferralScreenState
initData =
  { referralCode: ""
  , btnActive: false
  , showThanks: false
  , isInvalidCode: false
  , isExpandReference: false
  , config : getAppConfig appConfig
  , logField : empty
  , referralType : GET_REFERRED
  , showQRCodePopUp : false
  , referralComponentProps : {  stage : NO_REFERRAL_STAGE
                              , referralCode : Nothing
                              , applyButtonActive : false 
                              , showReferredUserInfoPopup : false
                              , showReferralProgramInfoPopup : false
                              , isInvalidCode : false
                              , isFocused : false
                             }
  }
