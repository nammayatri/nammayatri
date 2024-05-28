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
  = Decline Int
  | Accept Int
  | NoAction
  | AfterRender
  | NotificationLister String String
  | UpdateCarousel Int
  | AppendRequest (Array SearchRequest)
  | NotifyExpired (Array String)
  | OnPageSelected String

instance showAction :: Show Action where
  show _ = "BackClick"

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog