module Screens.RideRequestPopUp.Controller (module Screens.RideRequestPopUp.Controller, module ActionType) where

import Prelude

import Api.Types (NearBySearchRequestRes(..), SearchRequest(..))
import Data.Array (foldr, nub, null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Helpers.Commons (flowRunner, liftFlow)
import Presto.Core.Types.Language.Flow (Flow)
import PrestoDOM (Eval, continue, continueWithCmd, exit, getPushFn, update)
import Screens.RideRequestPopUp.ScreenData (RideRequestPopUpScreenData)
import Screens.RideRequestPopUp.TransFormer (toPopupProp)
import Screens.TopPriceView.Controller as TopPriceView
import Services.Backend (nearBySearchRequest)
import Types (Action(..)) as ActionType
import Types (Action(..), OverlayData(..), defaultOverlayData)

-- Controller
-- All actions which can be performed on the screen. Sample click includes NextClick and BackClick.
-- P.S. This is not the actual logic for going to next screen or previous screen. This is just a example
-- for showing 2 kinds of exits from the screen.
data ScreenOutput
  = NextScreen RideRequestPopUpScreenData
  | Back RideRequestPopUpScreenData

eval :: Action -> RideRequestPopUpScreenData -> Eval Action ScreenOutput RideRequestPopUpScreenData
eval (UpdateRideRequest searchData) state =
  if null searchData then 
    exit $ Back state
    else
    continueWithCmd state { holderData = toPopupProp searchData, rideRequests = searchData }
      [ do
          notifyTopView (TopPriceView.UpdateRideRequest searchData)
          pure NoAction
      ]

eval (AppendRequest request) state = do
  -- let updatedRequest = foldr (\item acc -> zcc) state.rideRequests state.rideRequests
  let popup = toPopupProp request
      updatedState = state { holderData = state.holderData <> popup, rideRequests = nub $  state.rideRequests <> request }
  continueWithCmd updatedState
    [ do
        notifyTopView $ TopPriceView.AppendRequest updatedState.rideRequests
        pure NoAction
    ]

eval NextClick state = exit $ NextScreen state

eval BackClick state = exit $ Back state

eval NoAction state = continue state

eval (Decline idx) state = exit $ Back state

eval (UpdateCarousel idx) state = continue state{selectedRequest = idx}

eval (NotificationLister nType id) state 
  | nType == "CLEARED_FARE" || nType == "CANCELLED_SEARCH_REQUEST" = do 
    let updatedRequests = foldr (\(SearchRequest item) acc -> if item.searchTryId == id then acc else acc <> [(SearchRequest item)]) [] state.rideRequests
    continueWithCmd state [(pure $ UpdateRideRequest updatedRequests)]
  | otherwise =  continueWithCmd state [ (do
                push <- getPushFn (Just "RideRequestPopUp") "RideRequestPopUp"
                fiber <- launchAff $ flowRunner defaultOverlayData $ getRideRequest push id
                pure $ NoAction
            )]

eval _ state = update state

notifyTopView :: TopPriceView.Action -> Effect Unit
notifyTopView action = do
  push <- getPushFn (Just "TopPriceView") "TopPriceView"
  void $ push action


getRideRequest :: (Action -> Effect Unit) -> String -> Flow OverlayData Unit
getRideRequest push id = do
  eiResp <- nearBySearchRequest id
  case eiResp of
    Right (NearBySearchRequestRes resp) -> do 
      liftFlow $ push $ AppendRequest resp.searchRequestsForDriver
    Left err -> do
      let _ = spy "Left err ->" err
      pure unit