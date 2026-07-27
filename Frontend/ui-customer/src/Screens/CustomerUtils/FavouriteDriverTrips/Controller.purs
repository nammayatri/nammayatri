module Screens.CustomerUtils.FavouriteDriverTrips.Controller where

import Prelude

import Common.Types.App (CategoryListType, ProviderType(..))
import Components.GenericHeader as GenericHeaderController
import Components.PopUpModal as PopUpModalController
import Components.PrimaryButton as PrimaryButtonController
import Components.SourceToDestination as SourceToDestinationController
import Data.Array (null)
import Data.String (length)
import Data.String (trim)
import JBridge (hideKeyboardOnNavigation, copyToClipboard)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent, trackAppTextInput)
import Prelude (class Show, pure, unit, not, bind, ($), (>), discard, void, (==))
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (FavouriteDriverTripsState, FavouriteDriverTripsType, Details(..))
import Services.API(FavouriteDriverRides(..), FavouriteDriverTripsResp(..))
import Engineering.Helpers.Utils as EHU
import Data.Lens ((^.))
import Accessor (_list)
import Debug

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = case action of
        _ -> pure unit

data Action = GenericHeaderActionController GenericHeaderController.Action
            | SourceToDestinationActionController SourceToDestinationController.Action 
            | BackPressed
            | MessageTextChanged String
            | ViewInvoice
            | AfterRender
            | NoAction
            | Copy
            | ShowPopUp
            | PopUpModalAction PopUpModalController.Action
            | ListExpandAinmationEnd
            | GetFavouriteDriversTripsAPIResponseAction (FavouriteDriverTripsResp)
            | RemoveFav
            | SavedLocation
            | GoToDriverProfile

data ScreenOutput = GoBack | GoToSavedLocation FavouriteDriverTripsState | GoToFavDriverProfile FavouriteDriverTripsState

eval :: Action -> FavouriteDriverTripsState -> Eval Action ScreenOutput FavouriteDriverTripsState

eval BackPressed state = exit $ GoBack 

eval SavedLocation state = exit $ GoToSavedLocation state

eval GoToDriverProfile state = exit $ GoToFavDriverProfile state

eval (GetFavouriteDriversTripsAPIResponseAction (FavouriteDriverTripsResp respList)) state = continue state{data{details = getFavouriteDriverList(respList.list)}}

eval (RemoveFav) state = do continueWithCmd state [do pure SavedLocation]

eval (GenericHeaderActionController (GenericHeaderController.PrefixImgOnClick )) state = continueWithCmd state [do pure BackPressed]

eval _ state = update state

getFavouriteDriverList :: (Array FavouriteDriverRides) -> Array Details 
getFavouriteDriverList (savedLocation) = (map (\(FavouriteDriverRides item) -> 
  { 
    rideRating : item.rideRating
  , fromLocation : item.fromLocation
  , toLocation : item.toLocation
  , totalFare : item.totalFare
  , startTime : item.startTime
  , id : item.id
  }
  )savedLocation)