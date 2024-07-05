{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RateCardScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard, void)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.RateCardScreen.Controller (ScreenOutput(..))
import Screens.RateCardScreen.View as RateCardScreen
import Types.App (FlowBT, GlobalState(..), RATE_CARD_SCREEN_OUTPUT(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import Screens.RateCardScreen.RateCardBottomScreen
import Presto.Core.Types.Language.Flow (fork)


rateCardScreen :: FlowBT String RATE_CARD_SCREEN_OUTPUT
rateCardScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ RateCardScreen.screen state.rateCardScreen
  case action of
    UpdatePrice updatedState val -> do
      modifyScreenState $ RateCardScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ RATE_CARD_API updatedState val)
    OpenRateCard updateState pref -> do
      modifyScreenState $ RateCardScreenStateType (\_ -> updateState)
      lift $ lift $ void $ fork $ showRateCard {serviceTier : updateState.data.rateCard.serviceTierName, rateCardData : updateState.data.rateCard, ridePreference :pref, animate :true}
      rateCardScreen 
    _ -> App.BackT $ pure App.GoBack