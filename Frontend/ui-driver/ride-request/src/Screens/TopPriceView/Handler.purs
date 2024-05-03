module Screens.TopPriceView.Handler where

import Prelude

import Api.Types (SearchRequest(..))
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Helpers.Accessor (_rideRequestPopUpScreen)
import Helpers.Commons (liftFlow)
import Presto.Core.Types.Language.Flow (Flow, getState, modifyState)
import PrestoDOM (getPushFn, showScreenWithNameSpace)
import PrestoDOM.Core (initUIWithNameSpace)
import PrestoDOM.List (preComputeListItem)
import Screens.RideRequestPopUp.Controller (Action)
import Screens.TopPriceView.Controller (ScreenOutput)
import Screens.TopPriceView.View as View
import Types (OverlayData(..))

topPriceView :: Array SearchRequest -> Flow OverlayData Unit
topPriceView searchRequests = do
  void $ liftFlow $ initUIWithNameSpace "TopPriceView" (Just "TopPriceView") 
  OverlayData oState <- modifyState \(OverlayData oState) -> OverlayData $ oState{topPriceViewState{rideRequests = searchRequests}}
  void $ showScreenWithNameSpace $ View.screen (OverlayData oState)
