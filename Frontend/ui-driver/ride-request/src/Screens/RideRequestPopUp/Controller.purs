module Screens.RideRequestPopUp.Controller (module Screens.RideRequestPopUp.Controller, module ActionType) where

import Prelude

import Api.Types (NearBySearchRequestRes(..), SearchRequest(..))
import Control.Monad.State (state)
import Data.Array (elem, filter, foldr, notElem, nub, null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Helpers.Commons (flowRunner, liftFlow)
import Presto.Core.Types.Language.Flow (Flow)
import PrestoDOM (Eval, continue, continueWithCmd, exit, getPushFn, id, update)
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

eval (NotifyExpired ids) state = do
  if null ids then update state
    else do
      let onGoingRequest = filter (\(SearchRequest item) ->  notElem item.searchTryId ids) state.rideRequests
      if null onGoingRequest then exit $ Back state
        else do 
          let updatedState = state { holderData = toPopupProp onGoingRequest, rideRequests = onGoingRequest }
          continueWithCmd updatedState [ do
              notifyTopView $ TopPriceView.AppendRequest updatedState.rideRequests
              pure NoAction
          ]

eval (AppendRequest request) state = do
  let updatedRequest = spy "AppendRequest -> " $ foldr (\(SearchRequest item1) acc -> acc <> map (\(SearchRequest item2) -> if item2.searchTryId /= item1.searchTryId then (SearchRequest item2) else  (SearchRequest item1)) state.rideRequests) state.rideRequests request
      updatedState = state { holderData = toPopupProp updatedRequest, rideRequests = updatedRequest }
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
  | nType == "CLEARED_FARE" || nType == "CANCELLED_SEARCH_REQUEST" = continueWithCmd state [(pure $ NotifyExpired [id])]
  | otherwise =  continue state 
      -- continueWithCmd state [ (do
      --           push <- getPushFn (Just "RideRequestPopUp") "RideRequestPopUp"
      --           void $ pure $ launchAff $ flowRunner defaultOverlayData $ getRideRequest push id
      --           pure $ NoAction
      --       )]

notifyTopView :: TopPriceView.Action -> Effect Unit
notifyTopView action = do
  push <- getPushFn (Just "TopPriceView") "TopPriceView"
  void $ push action

