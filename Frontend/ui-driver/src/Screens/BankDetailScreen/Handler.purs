{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.BankDetailScreen.Handler
  ( bankDetail
  )
  where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>))
import Screens.BankDetailScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Screens.BankDetailScreen.View as BankDetailScreen
import Types.App (FlowBT, GlobalState(..), BANK_DETAILS_SCREENOUTPUT(..))
import React.Navigation.Navigate (navigateToScreen)


bankDetail :: FlowBT String BANK_DETAILS_SCREENOUTPUT
bankDetail = do
  (GlobalState state) <- getState
  action <- lift $ lift $ navigateToScreen $ BankDetailScreen.screen state.bankDetailsScreen
  case action of
    GoBack -> App.BackT $ pure App.GoBack
    GoToAddVehicleDetails updatedState -> App.BackT $ App.BackPoint <$> (pure $ GO_TO_ADD_VEHICLE_DETAILS)