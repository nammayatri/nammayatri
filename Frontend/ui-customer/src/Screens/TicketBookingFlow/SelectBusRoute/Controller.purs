module Screens.SelectBusRoute.Controller where

import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (void, class Show, discard, pure, unit, bind, ($), not, (+), (-), (==), (*), (<>), show, (+), (/=), (-), show, map, (<#>), const , (&&))
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
import Data.Array (length, (!!), catMaybes, filter,find, concatMap, mapMaybe)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Engineering.Helpers.Commons(getNewIDWithTag)
import JBridge (shareImageMessage, copyToClipboard, toast, firebaseLogEvent)
import Common.Types.App as Common
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.SelectBusRoute.ScreenData (SelectBusRouteScreenState)
import Services.API (FrfsQuote(..), FRFSRouteAPI(..), BusTrackingRouteResp(..), VehicleInfo(..))
import Helpers.FrfsUtils (getFirstRoute, calculateEtaForBusRoutes)
import Data.Foldable (minimum)
import Control.Bind ((>>=))

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    _ -> pure unit
    
data Action = AfterRender
            | NoAction
            | BackPressed
            | GenericHeaderAC GenericHeader.Action
            | SeeRouteButtonAction
            | UpdateQuotes (Array FrfsQuote)
            | SelectQuote FrfsQuote
            | EditStops
            | SortByPressed
            | UpdateEtaForRoutes String (Array VehicleInfo)
            | UpdateFastestRoute

data ScreenOutput = TrackBus SelectBusRouteScreenState
                  | GoBack

eval :: Action -> SelectBusRouteScreenState -> Eval Action ScreenOutput SelectBusRouteScreenState

eval BackPressed state = exit GoBack

eval EditStops state = continueWithCmd state [do pure BackPressed]

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure BackPressed]

eval (SeeRouteButtonAction) state = do
  void $ pure $ firebaseLogEvent "ny_bus_user_stop_search_completed"
  exit $ TrackBus state 
  
eval (SelectQuote quote) state =
  continueWithCmd state { data{ selectedQuote = Just quote } } [do pure $ SeeRouteButtonAction]

eval (UpdateQuotes quotes) state = do
  let routes =
        catMaybes $  
          map (\quote ->
            getFirstRoute quote
          ) quotes
      cheapestRouteCode = getCheapestRouteBasedOnPrice quotes    
  continue state{ data{ quotes = Just quotes, cheapestRoute = cheapestRouteCode } }

eval SortByPressed state = do
 continue state{ data { isSortByPillClicked = true}}

eval (UpdateEtaForRoutes routeCode' vehicleData) state = do
  let etaListOfRoute = concatMap (\vehicleInfo -> calculateEtaForBusRoutes vehicleInfo state.data.srcLoc) vehicleData
      minimumEta = minimum $ catMaybes etaListOfRoute
      updatedEta = state.data.eta <> [{ etas: minimumEta, routeCode: routeCode' }]
  continueWithCmd state{ data { eta = updatedEta } } [do pure $ UpdateFastestRoute]

eval (UpdateFastestRoute) state = do
  let
    justEtas = filter (\x -> x.etas /= Nothing) state.data.eta
    minEta = minimum (mapMaybe _.etas justEtas)
    minEtaRecords =
      case minEta of
        Nothing   -> []
        Just mEta -> filter (\x -> x.etas == Just mEta) justEtas
  case minEtaRecords of
    [onlyOne] ->
      continue state { data { fastestRoute = Just onlyOne.routeCode } }
    _ ->
      continue state

eval _ state = continue state


getCheapestRouteBasedOnPrice :: Array FrfsQuote -> Maybe String
getCheapestRouteBasedOnPrice quotes = do
  let cheapestPrice = fromMaybe 0.0 $ minimum $ map (\(FrfsQuote q) -> q.price) quotes
      cheapestFRFSQuote = filter (\(FrfsQuote q) -> q.price == cheapestPrice) quotes
  case cheapestFRFSQuote of
    [(FrfsQuote route)] ->
      case getFirstRoute (FrfsQuote route) of
        Just (FRFSRouteAPI routeDetails) -> Just routeDetails.shortName
        Nothing -> Nothing
    _ -> Nothing

    
