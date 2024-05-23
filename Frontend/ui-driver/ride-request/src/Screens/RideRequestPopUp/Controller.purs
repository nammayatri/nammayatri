module Screens.RideRequestPopUp.Controller (module Screens.RideRequestPopUp.Controller, module ActionType) where

import Prelude

import Api.Types (OfferType(..)) as OfferTypes
import Api.Types (SearchRequest(..))
import Control.Monad.State (state)
import Data.Array (filter, foldr, index, notElem, nubByEq, null)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Uncurried (runEffectFn1)
import Helpers.Commons (callDecline, flowRunner)
import PrestoDOM (Eval, continue, continueWithCmd, exit, getPushFn, update)
import Screens.RideRequestPopUp.ScreenData (RideRequestPopUpScreenData)
import Screens.RideRequestPopUp.TransFormer (toPopupProp)
import Screens.TopPriceView.Controller as TopPriceView
import Services.Backend (mkQuoteOffer, quoteOfferApi)
import Types (Action(..)) as ActionType
import Types (Action(..), defaultOverlayData)

data ScreenOutput
  = Back RideRequestPopUpScreenData
  | AcceptRequest SearchRequest

eval :: Action -> RideRequestPopUpScreenData -> Eval Action ScreenOutput RideRequestPopUpScreenData

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
  let updatedRequest = nubByEq (\(SearchRequest item1) (SearchRequest item2) -> item1.searchTryId == item2.searchTryId) $ state.rideRequests <> request
      updatedState = state { holderData = toPopupProp updatedRequest, rideRequests = updatedRequest }
  continueWithCmd updatedState
    [ do
        notifyTopView $ TopPriceView.AppendRequest updatedState.rideRequests
        pure NoAction
    ]

eval NoAction state = continue state

eval (Decline idx) state = 
  case index state.rideRequests idx of
    Nothing -> continue state
    Just (SearchRequest item) -> continueWithCmd state 
                  [ do  
                    void $ runEffectFn1 callDecline item.searchTryId
                    notifyTopView $ TopPriceView.NotifyExpired [item.searchTryId]
                    pure $ NotifyExpired [item.searchTryId]]

eval (Accept idx) state = 
  case index state.rideRequests idx of
    Nothing -> continue state
    Just item -> exit $ AcceptRequest item

eval (UpdateCarousel idx) state = continue state{selectedRequest = idx}

eval (OnPageSelected idx) state = case fromString idx of 
  Nothing -> update state
  Just item -> continueWithCmd state{selectedRequest = item}
                [do 
                  _ <- notifyTopView $ TopPriceView.OnCardChanged item
                  pure NoAction]

eval (NotificationLister nType id) state 
  | nType == "CLEARED_FARE" || nType == "CANCELLED_SEARCH_REQUEST" = continueWithCmd state [(pure $ NotifyExpired [id])]
  | otherwise =  continue state 

notifyTopView :: TopPriceView.Action -> Effect Unit
notifyTopView action = do
  push <- getPushFn (Just "TopPriceView") "TopPriceView"
  void $ push action

-- void $ quoteOfferApi $ mkAcceptQuote item.searchTryId