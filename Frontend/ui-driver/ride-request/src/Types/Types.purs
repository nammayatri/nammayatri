module Types where

import Screens.RideRequestPopUp.ScreenData (RideRequestPopUpScreenData, initData)

newtype OverlayData
  = OverlayData
  { rideRequestPopUpScreen :: RideRequestPopUpScreenData
  }

defaultOverlayData :: OverlayData
defaultOverlayData =
  OverlayData
    { rideRequestPopUpScreen: initData
    }

data LazyCheck
  = TypoGraphy
