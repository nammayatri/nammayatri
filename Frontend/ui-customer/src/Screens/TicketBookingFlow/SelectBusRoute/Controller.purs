module Screens.SelectBusRoute.Controller where

import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (void, class Show, discard, pure, unit, bind, ($), not, (+), (-), (==), (*), (<>), show, (+), (==), (-), show, map)
import PrestoDOM (Eval, update, continue, exit, updateAndExit, continueWithCmd, continueWithCmd)
import Screens (ScreenName(..), getScreen)
import PrestoDOM.Types.Core (class Loggable)
import Helpers.Utils (compareDate, getCurrentDate, generateQR)
import Effect.Uncurried (runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Effect.Uncurried(runEffectFn4)
import Debug (spy)
import Data.Array (length, (!!), catMaybes)
import Data.Maybe (Maybe(..), maybe)
import Engineering.Helpers.Commons(getNewIDWithTag)
import JBridge (shareImageMessage, copyToClipboard, toast, firebaseLogEvent)
import Common.Types.App as Common
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.SelectBusRoute.ScreenData (SelectBusRouteScreenState)
import Services.API (FrfsQuote(..), FRFSRouteAPI(..))
import Helpers.FrfsUtils (getFirstRoute)
import Engineering.Helpers.Events as Events

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    _ -> pure unit
    
data Action = AfterRender
            | NoAction
            | BackPressed
            | GenericHeaderAC GenericHeader.Action
            | SeeRouteButtonAction PrimaryButton.Action
            | UpdateQuotes (Array FrfsQuote)
            | SelectQuote FrfsQuote
            | EditStops

data ScreenOutput = TrackBus SelectBusRouteScreenState
                  | GoBack

eval :: Action -> SelectBusRouteScreenState -> Eval Action ScreenOutput SelectBusRouteScreenState

eval BackPressed state = exit GoBack

eval EditStops state = continueWithCmd state [do pure BackPressed]

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure BackPressed]

eval (SeeRouteButtonAction (PrimaryButton.OnClick)) state = do
  let _ = unsafePerformEffect $ Events.addEventAggregate "ny_bus_user_stop_search_completed"
  exit $ TrackBus state 
  
eval (SelectQuote quote) state =
  continue state{ data{ selectedQuote = Just quote } }

eval (UpdateQuotes quotes) state = do
  let routes =
        catMaybes $  
          map (\quote ->
            getFirstRoute quote
          ) quotes
  continue state{ data{ quotes = Just quotes } }

eval _ state = continue state