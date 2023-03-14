module Screens.BankDetailScreen.Handler
  ( bankDetail
  )
  where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>))
import Screens.BankDetailScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.BankDetailScreen.View as BankDetailScreen
import Types.App (FlowBT, GlobalState(..), BANK_DETAILS_SCREENOUTPUT(..))


bankDetail :: FlowBT String BANK_DETAILS_SCREENOUTPUT
bankDetail = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ BankDetailScreen.screen state.bankDetailsScreen
  case action of
    GoBack -> App.BackT $ pure App.GoBack
    GoToAddVehicleDetails updatedState -> App.BackT $ App.BackPoint <$> (pure $ GO_TO_ADD_VEHICLE_DETAILS)