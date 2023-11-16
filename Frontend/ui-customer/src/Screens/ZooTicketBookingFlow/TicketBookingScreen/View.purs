module Screens.TicketBookingScreen.View where

import Animation as Anim 
import Animation.Config (translateYAnimConfig, translateYAnimMapConfig, removeYAnimFromTopConfig)
import JBridge as JB 
import Prelude (Unit, bind, const, pure, unit, ($), (&&), (/=), (<<<),(<>), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, afterRender, background, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, orientation, padding, scrollView, text, textSize, textView, weight, width, imageWithFallback)
import PrestoDOM.Animation as PrestoAnim
import Screens.TicketBookingScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App
import Screens.TicketBookingScreen.ComponentConfig 
import Effect (Effect)
import Components.GenericHeader as GenericHeader

screen :: ST.TicketBookingScreenState -> Screen Action ST.TicketBookingScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "ChooseLanguageScreen"
  , globalEvents : []
  , eval
  }

view :: forall w . (Action -> Effect Unit) -> ST.TicketBookingScreenState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout 
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  ]([GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)] 
    <> if state.props.currentStage == ST.DescriptionStage then [ descriptionView state push ]
      else if state.props.currentStage == ST.ChooseTicketStage then [ chooseTicketsView state push ]
      else if state.props.currentStage == ST.BookingConfirmationStage then [ bookingConfirmationView state push ]
      else if state.props.currentStage == ST.ViewTicketStage then [ ticketInfoView state push ]
      else [])

descriptionView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
descriptionView state push = 
  linearLayout[][]

chooseTicketsView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chooseTicketsView state push = 
  linearLayout[][]

bookingConfirmationView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
bookingConfirmationView state push = 
  linearLayout[][]

ticketInfoView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
ticketInfoView state push = 
  linearLayout[][]