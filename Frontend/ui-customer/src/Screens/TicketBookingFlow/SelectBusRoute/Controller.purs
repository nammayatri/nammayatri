module Screens.SelectBusRoute.Controller where

import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (void, class Show, Ordering(..), discard, pure, unit, bind, ($), not, (+), (-), (==), (*), (<>), show, (+), (/=), (-), show, map, (<#>), const , (&&))
import Prelude (void, class Show, Ordering(..), discard, pure, unit, bind, ($), not, (+), (-), (==), (*), (<>), show, (+), (/=), (-), show, map, (<#>), const , (&&))
import PrestoDOM (Eval, update, continue, exit, updateAndExit, continueWithCmd, continueWithCmd)
import Screens (ScreenName(..), getScreen)
import PrestoDOM.Types.Core (class Loggable)
import Helpers.Utils (getCurrentDate, generateQR)
import Effect.Uncurried (runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Effect.Uncurried(runEffectFn4)
import Debug (spy)
import Data.Array (length, (!!), catMaybes, filter,find, concatMap, mapMaybe, sortBy, reverse)
import Data.Ord (compare)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Ord (compare)
import Engineering.Helpers.Commons(getNewIDWithTag)
import Engineering.Helpers.Utils(compareDate)
import JBridge (shareImageMessage, copyToClipboard, toast, firebaseLogEvent)
import Common.Types.App as Common
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.SelectBusRoute.ScreenData (SelectBusRouteScreenState, EtaBasedOnRoute(..))
import Services.API (FrfsQuote(..), FRFSRouteAPI(..), BusTrackingRouteResp(..), VehicleInfo(..),FRFSVehicleServiceTierAPI(..))
import Helpers.FrfsUtils (getFirstRoute, calculateEtaForBusRoutes)
import Data.Foldable (minimum)
import Control.Bind ((>>=))
import Engineering.Helpers.Events as Events
import Engineering.Helpers.LogEvent (logEvent)
import Foreign.Object (empty)

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
            | ClosePopupButtonAction
            | SortByOptionPressed STR 
            | InitialRenderDone

data ScreenOutput = TrackBus SelectBusRouteScreenState
                  | GoBack

eval :: Action -> SelectBusRouteScreenState -> Eval Action ScreenOutput SelectBusRouteScreenState

eval BackPressed state = exit GoBack

eval EditStops state = continueWithCmd state [do pure BackPressed]

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure BackPressed]

eval SeeRouteButtonAction state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_bus_user_stop_search_completed"
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
  continue state{ data{ quotes = Just quotes, cheapestRoute = cheapestRouteCode }, props{isInitialRender = false} }

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

eval ClosePopupButtonAction state = do
  continue state { data { isSortByPillClicked = false, sortbyPillText = getString SORT_BY } }

eval (SortByOptionPressed sortByText) state = do  
  let sortedQuotes =
        case sortByText of
          AC_BUS -> getSortedQuotesBasedOnServiceTier state.data.quotes
          EARLY_DEPARTURE -> getSortedQuotesBasedOnEta state.data.quotes state.data.eta
          _ -> getSortedRoutesBasedOnPrice state.data.quotes
  continue state { data { isSortByPillClicked = false, sortbyPillText = getString sortByText, quotes = Just sortedQuotes } }

eval InitialRenderDone state = continue state{props{isInitialRender = false}}

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

    
getSortedQuotesBasedOnServiceTier :: Maybe (Array FrfsQuote) -> Array FrfsQuote
getSortedQuotesBasedOnServiceTier quotes = 
  let sortedQuotes = sortBy compareServiceTierType $ fromMaybe [] quotes
      compareServiceTierType (FrfsQuote q1) (FrfsQuote q2) =
        let
          firstRoute1 = getFirstRoute (FrfsQuote q1)
          firstRoute2 = getFirstRoute (FrfsQuote q2)
        in
          compare (priority (getRoute firstRoute1))
                  (priority (getRoute firstRoute2))
     
      getRoute (Just (FRFSRouteAPI route)) = getType route.vehicleServiceTier
      getRoute Nothing = ""

      getType :: Maybe FRFSVehicleServiceTierAPI -> String
      getType (Just (FRFSVehicleServiceTierAPI v)) = v._type
      getType Nothing = "" 

      priority :: String -> Int
      priority "AC" = 0
      priority _ = 1
  in sortedQuotes

getSortedQuotesBasedOnEta :: Maybe (Array FrfsQuote) -> Array EtaBasedOnRoute -> Array FrfsQuote
getSortedQuotesBasedOnEta quotes eta = 
  let sortedEta = sortBy (\a b -> compareEta a.etas b.etas) eta

      compareEta :: Maybe Int -> Maybe Int -> Ordering
      compareEta (Just x) (Just y) = compare x y         
      compareEta (Just _) Nothing  = LT                 
      compareEta Nothing (Just _)  = GT                 
      compareEta Nothing Nothing   = EQ

      
      sortedQuotes = mapMaybe (\eta -> findQuoteByRouteCode eta.routeCode $ fromMaybe [] quotes) sortedEta
      findQuoteByRouteCode routeCode quotes =
        let
          quote = find (\(FrfsQuote q) -> getRouteCode (getFirstRoute (FrfsQuote q)) == routeCode) quotes

          getRouteCode (Just (FRFSRouteAPI route)) = route.shortName
          getRouteCode Nothing = ""
        in quote
  in sortedQuotes

getSortedRoutesBasedOnPrice :: Maybe (Array FrfsQuote) -> Array FrfsQuote
getSortedRoutesBasedOnPrice quotes =
  let sortedQuotes = sortBy comparePrice $ fromMaybe [] quotes
      comparePrice (FrfsQuote q1) (FrfsQuote q2) =
        let
          price1 = q1.price
          price2 = q2.price
        in
          compare price1 price2
  in sortedQuotes