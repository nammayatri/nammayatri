module Screens.BookingOptionsScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard, void)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.BookingOptionsScreen.Controller (ScreenOutput(..))
import Screens.BookingOptionsScreen.View as BookingOptionsScreen
import Types.App (BOOKING_OPTIONS_SCREEN_OUTPUT(..), FlowBT, GlobalState(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import Screens.RateCardScreen.RateCardBottomScreen
import Presto.Core.Types.Language.Flow (fork)

bookingOptions :: FlowBT String BOOKING_OPTIONS_SCREEN_OUTPUT
bookingOptions = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ BookingOptionsScreen.screen state.bookingOptionsScreen
  case action of
    ChangeRidePreference updatedState service -> do
      modifyScreenState $ BookingOptionsScreenType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ CHANGE_RIDE_PREFERENCE updatedState service)
    ToggleACAvailability updatedState toogleVal -> do
      modifyScreenState $ BookingOptionsScreenType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ UPDATE_AC_AVAILABILITY updatedState toogleVal)
    ToggleRentalIntercityRide updatedState -> do
      modifyScreenState $ BookingOptionsScreenType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ ENABLE_RENTAL_INTERCITY_RIDE updatedState)
    GoBack state -> do
      modifyScreenState $ BookingOptionsScreenType (\_ -> state{props{ fromDeepLink = false }})
      if state.props.fromDeepLink
        then App.BackT $ App.NoBack <$> pure HOME_SCREEN_FROM_BOOKING_PREFS
        else App.BackT $ pure App.GoBack
    ExitToRateCardScreen updatedState -> do
      modifyScreenState $ BookingOptionsScreenType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ EXIT_TO_RATE_CARD_SCREEN updatedState)
    OpenRateCard pref updatedState -> do
      modifyScreenState $ BookingOptionsScreenType (\_ -> updatedState)
      lift $ lift $ void $ fork $ showRateCard {serviceTier : updatedState.data.rateCard.serviceTierName, rateCardData : updatedState.data.rateCard, ridePreference :pref, animate : true}
      bookingOptions