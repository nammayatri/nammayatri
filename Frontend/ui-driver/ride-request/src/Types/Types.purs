module Types where

import Api.Types (SearchRequest)
import Screens.RideRequestPopUp.ScreenData (RideRequestPopUpScreenData, initData)
import Prelude
import PrestoDOM (class Loggable, defaultPerformLog)


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


data Action
  = NextClick
  | BackClick
  | Decline Int
  | UpdateRideRequest (Array SearchRequest)
  | NoAction
  | NotificationLister String String
  | UpdateCarousel Int
  | AppendRequest (Array SearchRequest)

instance showAction :: Show Action where
  show _ = "BackClick"

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog