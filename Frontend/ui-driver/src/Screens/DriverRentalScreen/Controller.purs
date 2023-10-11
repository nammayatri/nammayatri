module Screens.DriverRentalScreen.Controller where

import Prelude

import Debug

import Components.PopUpModal as PopUpModal
import Components.PrimaryButton.Controller as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), length, replace, replaceAll)
import Data.String.CodeUnits (charAt)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (getNewIDWithTag, setText)
import JBridge (requestKeyboardShow, hideKeyboardOnNavigation, minimizeApp)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, bind, discard, not, pure, unit, when, ($), (&&), (<=), (==), (>), (||), (<>), (+), show)
import PrestoDOM (Eval, continue, continueWithCmd, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.AddVehicleDetailsScreen.Controller (dateFormat)
import Screens.Types (DriverRentalScreenState)

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = case action of
        AfterRender -> trackAppScreenRender appId "screen" (getScreen DRIVER_RENTAL_SCREEN)
        BackPressed -> do 
            trackAppBackPress appId (getScreen DRIVER_RENTAL_SCREEN)
            trackAppEndScreen appId (getScreen DRIVER_RENTAL_SCREEN)
        NAVIGATE_TO_PICKUP -> trackAppScreenRender appId "screen" (getScreen DRIVER_RENTAL_SCREEN)


data ScreenOutput = GoToHomeScreen DriverRentalScreenState | GoBack


data Action = BackPressed | AfterRender | NAVIGATE_TO_PICKUP

eval :: Action -> DriverRentalScreenState -> Eval Action ScreenOutput DriverRentalScreenState
eval action state = case action of 
    AfterRender -> continue state
    BackPressed -> exit $ GoBack 
    NAVIGATE_TO_PICKUP -> continue state {props {isRentalAccepted = true}}
    _  -> continue state
