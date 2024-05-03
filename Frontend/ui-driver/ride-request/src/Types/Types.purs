module Types where

import Screens.RideRequestPopUp.ScreenData (RideRequestPopUpScreenData, initData)
import Screens.TopPriceView.ScreenData (TopPriceViewState, initData) as TopPriceViewState

newtype OverlayData
  = OverlayData
  { rideRequestPopUpScreen :: RideRequestPopUpScreenData
  , topPriceViewState:: TopPriceViewState.TopPriceViewState
  }

defaultOverlayData :: OverlayData
defaultOverlayData =
  OverlayData
    { rideRequestPopUpScreen: initData
    , topPriceViewState: TopPriceViewState.initData
    }

data LazyCheck
  = TypoGraphy
