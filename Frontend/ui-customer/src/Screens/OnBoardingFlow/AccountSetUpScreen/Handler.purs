{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AccountSetUpScreen.Handler where

import Prelude (bind, ($), pure , (<$>), void, discard)
import ModifyScreenState (modifyScreenState)
import Types.App (ScreenType(..))
import Engineering.Helpers.BackTrack (getState)
import Screens.AccountSetUpScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.AccountSetUpScreen.View as AccountSetUpScreen
import Types.App (FlowBT, GlobalState(..),ACCOUNT_SET_UP_SCREEN_OUTPUT(..))
import Storage (KeyStore(..), setValueToLocalStore)
import Engineering.Helpers.Events as EHE


accountSetUpScreen ::FlowBT String ACCOUNT_SET_UP_SCREEN_OUTPUT
accountSetUpScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ AccountSetUpScreen.screen state.accountSetUpScreen
  _ <- setValueToLocalStore CUSTOMER_FIRST_SIGNUP "true"
  let _ = EHE.addEvent (EHE.defaultEventObject "profile_details_page_loaded") { module = "onboarding"}
  case act of
    GoHome updatedState ->  App.BackT $ App.NoBack <$> (pure $ GO_HOME updatedState)
    ChangeMobileNumber -> App.BackT $ App.NoBack <$> (pure $ GO_BACK)
    VerifyReferral state  -> do
      void $ modifyScreenState $ AccountSetUpScreenStateType (\_ -> state)
      App.BackT $ App.NoBack <$> (pure $ APPLY_REFERRAL state.data.referralCode)