module Screens.DriverRentalScreen.Handler where

import Prelude


import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Data.Maybe (isJust)
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.DriverRentalScreen.Controller (ScreenOutput(..))
import Screens.DriverRentalScreen.ScreenData as DriverRentalScreenData

import Screens.DriverRentalScreen.View as DriverRentalScreen
import Screens.Types (RentalRequestDetial(..))
import Types.App (FlowBT, GlobalState(..), NAVIGATION_ACTIONS(..), RENTAL_SCREEN_OUTPUT(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Common.Types.App (LazyCheck(..))


rentalScreen :: FlowBT String RENTAL_SCREEN_OUTPUT
rentalScreen = do
    (GlobalState state) <- getState
    act <- lift $ lift $ runScreen $ DriverRentalScreen.screen state.driverRentalScreen
    case act of
      GoToHomeScreen updatedState -> do
        App.BackT $ App.NoBack <$> (pure $ GO_TO_HOME_SCREEN_FROM_RENTAL_SCREEN)
      GoBack -> App.BackT $ pure App.GoBack

