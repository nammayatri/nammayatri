module Screens.TicketInfoScreen.Controller where

import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (void, class Show, discard, pure, unit, bind, ($), not, (+), (-), (==), (*), (<>), show, (+), (==), (-), show)
import PrestoDOM (Eval, update, continue, exit, updateAndExit, continueWithCmd, continueWithCmd)
import Screens (ScreenName(..), getScreen)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (TicketInfoScreenState)
import Helpers.Utils (getCurrentDate)
import Engineering.Helpers.Utils(compareDate)
import Effect.Uncurried (runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Effect.Uncurried(runEffectFn4)
import Debug (spy)
import Helpers.Utils (generateQR, emitTerminateApp, isParentView)
import Data.Array (length, (!!))
import Data.Maybe (Maybe(..), maybe)
import Engineering.Helpers.Commons(getNewIDWithTag)
import Components.GenericHeader as GenericHeader
import JBridge (shareImageMessage, copyToClipboard)
import Engineering.Helpers.Utils as EHU
import Common.Types.App as Common
import Services.API (TicketPlaceResp(..))
import Language.Strings (getString)
import Language.Types (STR(..))

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
            | Copy String


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
  let textMessage = getTextMessage state.data.selectedBookingInfo.ticketPlaceName
  void $ pure $ shareImageMessage textMessage (shareImageMessageConfig serviceName)
  continue state

eval GoHome state = 
  if isParentView Common.FunctionCall
    then do
        void $ pure $ emitTerminateApp Nothing true
        continue state
  else exit GoToHomeScreen

eval (Copy text) state = continueWithCmd state [ do 
    void $ pure $ copyToClipboard text
    void $ pure $ EHU.showToast (getString COPIED)
    pure NoAction
  ]

eval _ state = update state

shareImageMessageConfig :: String -> Common.ShareImageConfig
shareImageMessageConfig serviceName = {
  code : "",
  viewId : getNewIDWithTag "QR_TICKET",
  logoId : getNewIDWithTag "ticketQRView",
  isReferral : false
  }

getTextMessage :: String -> String
getTextMessage placeName = case placeName of
  "Alipore Zoo" -> "Embark on a wild adventure at Alipore Zoo! 🐅🌿 Your tickets are ready to unlock a day of fun and discovery. See you soon! 🎟️👀 #AliporeZooAdventures"
  "Kolkata Heritage River Cruise" -> "Hey! Get ready to immerse yourself in the rich heritage and beauty of Kolkata with these tickets to the Millennium Heritage Cruise!"
  _ -> ""
