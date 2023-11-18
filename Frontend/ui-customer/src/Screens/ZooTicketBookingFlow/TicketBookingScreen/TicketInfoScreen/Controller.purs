module Screens.TicketInfoScreen.Controller where

import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, discard, pure, unit, bind, ($), not, (+), (-), (==), (*), (<>), show, (+), (==), (-), show)
import PrestoDOM (Eval, continue, exit, updateAndExit, continueWithCmd, continueWithCmd)
import Screens (ScreenName(..), getScreen)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (TicketInfoScreenState)
import Helpers.Utils (compareDate, getCurrentDate)
import Effect.Uncurried (runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Effect.Uncurried(runEffectFn4)
import Debug (spy)
import Helpers.Utils (generateQR)
import Data.Array (length, (!!))
import Data.Maybe (Maybe(..))
import Engineering.Helpers.Commons(getNewIDWithTag)
import Components.GenericHeader as GenericHeader
import JBridge (shareImageMessage)
import Common.Types.App as Common

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    _ -> pure unit
    
data Action = AfterRender
            | NoAction
            | BackPressed
            | TicketQRRendered String String
            | DecrementSliderIndex
            | IncrementSliderIndex
            | GenericHeaderAC GenericHeader.Action 
            | GoHome
            | ShareTicketQR String


data ScreenOutput = GoToHomeScreen | GoBack

eval :: Action -> TicketInfoScreenState -> Eval Action ScreenOutput TicketInfoScreenState
eval BackPressed state = exit GoBack

eval IncrementSliderIndex state = do
  let len = length state.data.selectedBookingInfo.services
      activeItem = state.data.selectedBookingInfo.services !! (state.props.activeIndex + 1)
  case activeItem of
    Just item -> continueWithCmd state {props{leftButtonDisable = false, rightButtonDisable = (state.props.activeIndex + 1) == (len-1), activeListItem = item, activeIndex = state.props.activeIndex + 1}} [ do
      pure $ (TicketQRRendered (getNewIDWithTag "ticketQRView") item.ticketServiceShortId )
    ]
    Nothing -> continue state

eval DecrementSliderIndex state = do
  let len = length state.data.selectedBookingInfo.services
      activeItem = state.data.selectedBookingInfo.services !! (state.props.activeIndex - 1)
  case activeItem of
    Just item -> continueWithCmd state{props{rightButtonDisable = false, leftButtonDisable = (state.props.activeIndex - 1) == 0, activeListItem = item, activeIndex = state.props.activeIndex - 1}} [ do
      pure $ (TicketQRRendered (getNewIDWithTag "ticketQRView") item.ticketServiceShortId )     
    ]
    Nothing -> continue state

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure BackPressed]

eval (TicketQRRendered id text) state  = 
  continueWithCmd state [ do
    runEffectFn4 generateQR text id 200 0
    pure $ NoAction
  ]

eval (ShareTicketQR serviceName) state = do
  _ <- pure $ shareImageMessage "Embark on a wild adventure at Alipore Zoo! 🐅🌿 Your tickets are ready to unlock a day of fun and discovery. See you soon! 🎟️👀 #AliporeZooAdventures" (shareImageMessageConfig serviceName)
  continue state

eval GoHome state = exit GoToHomeScreen

eval _ state = continue state

shareImageMessageConfig :: String -> Common.ShareImageConfig
shareImageMessageConfig serviceName = {
  code : "",
  viewId : getNewIDWithTag "QR_TICKET",
  logoId : getNewIDWithTag "ticketQRView",
  isReferral : false
  }
