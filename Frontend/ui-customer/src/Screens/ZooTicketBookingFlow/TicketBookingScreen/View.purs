module Screens.TicketBookingScreen.View where

import Animation as Anim 
import Animation.Config (translateYAnimConfig, translateYAnimMapConfig, removeYAnimFromTopConfig)
import JBridge as JB 
import Prelude (Unit, bind, const, pure, unit, ($), (&&), (/=), (<<<),(<>), (==), map)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..), PrestoDOM, Screen, afterRender, background, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, orientation, padding, scrollView, text, textSize, textView, weight, width, imageWithFallback, cornerRadius, relativeLayout, alignParentBottom, layoutGravity, stroke, visibility, textFromHtml, onClick)
import PrestoDOM.Animation as PrestoAnim
import Screens.TicketBookingScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App
import Screens.TicketBookingScreen.ComponentConfig 
import Effect (Effect)
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Font.Style as FontStyle
import Engineering.Helpers.Commons (screenWidth)
import Helpers.Utils (fetchImage, FetchImageFrom(..), decodeError, fetchAndUpdateCurrentLocation, getAssetsBaseUrl, getCurrentLocationMarker, getLocationName, getNewTrackingId, getPreviousVersion, getSearchType, parseFloat, storeCallBackCustomer)


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
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
   ][
    linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.white900
    , orientation VERTICAL
    , margin $ MarginBottom 90
    ][GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
    , scrollView[
        height WRAP_CONTENT
      , width MATCH_PARENT
      ][  linearLayout
          [height MATCH_PARENT
          , width MATCH_PARENT
          , gravity CENTER
          , margin $ MarginHorizontal 16 16 
          , orientation VERTICAL]
          ( if state.props.currentStage == ST.DescriptionStage then 
          [ imageView
            [ height $ V 370
            , width $ V 400
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_animal_1" 
            , margin $ MarginBottom 15
            , background Color.red
            ]
          , descriptionView state push ]
        else if state.props.currentStage == ST.ChooseTicketStage then [ chooseTicketsView state push ]
        else if state.props.currentStage == ST.BookingConfirmationStage then [ bookingConfirmationView state push ]
        else if state.props.currentStage == ST.ViewTicketStage then [ ticketInfoView state push ]
        else [])]
    ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , alignParentBottom "true,-1"
      , margin $ MarginBottom 16
      , orientation VERTICAL
      , background Color.white900
      ][  linearLayout  
          [height $ V 1
          , width MATCH_PARENT
          , margin $ MarginBottom 16
          , background Color.grey900][]
        , PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)
      ]
  ]

descriptionView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
descriptionView state push = 
  linearLayout[
    height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][  textView $ 
      [ text "Zoological Garden, Alipore"
      , color Color.black900
      ] <> FontStyle.h3 TypoGraphy 
    , textView $ 
      [ text "The Zoological Garden, Alipore is India's oldest formally stated zoological park (as opposed to royal and British menageries) and a big tourist attraction in Kolkata, West Bengal."
      , color Color.black800 
      ] <> FontStyle.body1 TypoGraphy 
    , locationView state push 
    , feeBreakUpView state push  
  ]


locationView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
locationView state push =
  linearLayout
  [ height WRAP_CONTENT 
  , width MATCH_PARENT
  , margin $ MarginTop 24
  , orientation VERTICAL
  ][  textView $ 
      [ text "Location"
      , color Color.black800
      , margin $ MarginBottom 8
      ] <> FontStyle.subHeading2 TypoGraphy
    , imageView
      [ height $ V 180
      , width $ V 328
      , cornerRadius 8.0 
      , layoutGravity "center_horizontal"
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_zoo_alipore_map" 
      ]
  ]

feeBreakUpView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
feeBreakUpView state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 8.0 
  , background Color.blue600
  , orientation VERTICAL 
  , padding $ Padding 20 20 20 20
  , margin $ MarginTop 24
  ][  textView $
      [ text "Fee & Zoo Hours"
      , color Color.black800
      ] <> FontStyle.subHeading2 TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ](map ( \item -> 
              linearLayout
                [ height WRAP_CONTENT
                , margin $ MarginTop 16
                , width MATCH_PARENT
                ][  imageView
                  [ height $ V 24 
                  , width $ V 24
                  , margin $ MarginRight 16
                  , imageWithFallback $ fetchImage FF_COMMON_ASSET item.image 
                  ]
                , linearLayout
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , orientation VERTICAL 
                  ][  textView $
                      [ text item.headingText 
                      , color Color.black800
                      ] <> FontStyle.subHeading2 TypoGraphy
                    , linearLayout[height WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation VERTICAL](map ( \subTextItem -> 
                      textView $ 
                      [ text subTextItem 
                      , color Color.black700 
                      , margin $ MarginTop 6
                      ] <> FontStyle.body1 TypoGraphy ) item.subtext)
                  ]                             
              ]
      ) (feeBreakUpArray)
    )

  ]

feeBreakUpArray :: Array {headingText :: String , subtext :: Array String, image :: String}
feeBreakUpArray = [{
  headingText : "Zoo Timings",
  subtext : ["Garden Time: 9:00 AM to 5:30 PM","Aquarium Time: 10:00 AM to 5:30 PM" ],
  image : "ny_ic_timing"
  },
  {
  headingText : "Entrance Fee",
  subtext : ["Up to the age of 5 years: ₹20" , "Visitors above the age of 5 years - ₹50"],
  image : "ny_ic_entry"

  },
  {
  headingText : "Aquarium Fee",
  subtext : ["Up to the age of 5 years - ₹10" , "Visitors above the age of 5 years - ₹20"],
  image : "ny_ic_aquarium"

  },
  {
  headingText : "Zoo Timings",
  subtext : ["You’ll be charged ₹250"],
  image : "ny_ic_videography"
  }
  ]

chooseTicketsView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chooseTicketsView state push = 
  linearLayout[
    height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][  textView $ 
      [ text "Date of Visit"
      , color Color.black900
      , margin $ MarginBottom 9
      ] <> FontStyle.subHeading2 TypoGraphy 
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , cornerRadius 8.0 
      , background Color.white900
      , stroke $ "1," <> Color.grey900
      , padding $ Padding 20 15 20 15
      , margin $ MarginBottom 20
      ][  imageView
          [ height $ V 24 
          , width $ V 24
          , margin $ MarginRight 8
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_calendar" 
          ]
        , textView $ 
          [ text "10/11/23"
          , color Color.black800
          ] <> FontStyle.body1 TypoGraphy
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ](map (\item -> ticketInputView { ticketType : item.title , ticketID : item.ticketID, ticketOption : item.ticketOption , isExpanded : item.isExpanded } push ) 
        [ {title : "Zoo Entry", ticketID : "ZOO_ENTRY" , ticketOption : zooEntryTicketOption state, isExpanded : state.data.zooEntry.availed}
        , {title : "Aquarium Entry", ticketID : "AQUARIUM_ENTRY" , ticketOption : aquariumEntryTicketOption state , isExpanded : state.data.aquariumEntry.availed}
        , {title : "Photo / Videography", ticketID : "PHOTO_OR_VIDEOGRAPHY" , ticketOption : photoVideographyTicketOption state , isExpanded : state.data.photoOrVideoGraphy.availed}
          ])
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity BOTTOM
      ][  imageView
          [ height $ V 16
          , width $ V 16 
          , layoutGravity "center_vertical"
          , margin $ MarginRight 8
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_checked" -- "ny_ic_unchecked
          ]
        , textView $ 
          [ text "I agree to the"
          , color Color.black800
          ] <> FontStyle.body1 TypoGraphy
        , textView $ 
          [ text " Terms & Conditions"
          , color Color.blue900
          ] <> FontStyle.body1 TypoGraphy
      ]
    , textView $ 
      [ textFromHtml " &#8226;&ensp; Cancellation of tickets is not applicable"
      , margin $ MarginTop 13
      , color Color.black700
      ] <> FontStyle.tags TypoGraphy
  ]

ticketInputView config push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 8.0 
  , background Color.blue600 
  , orientation VERTICAL
  , padding $ Padding 20 20 20 20
  , margin $ MarginBottom 20
  ][ linearLayout[
      height WRAP_CONTENT
    , width MATCH_PARENT
    , onClick push $ const (ToggleTicketOption config.ticketID)
    ][ textView $
        [ text config.ticketType --"Zoo Entry"
        , color Color.black800
        ] <> FontStyle.h2 TypoGraphy
      , linearLayout
        [weight 1.0][]
      , imageView 
        [ height $ V 20 
        , width $ V 20 
        , imageWithFallback $ fetchImage FF_COMMON_ASSET if config.isExpanded then "ny_ic_checked" else "ny_ic_unchecked" 
        ]
      ]
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , visibility if config.isExpanded then VISIBLE else GONE
        , orientation VERTICAL
        ](map (\item  -> incrementDecrementView item push) (config.ticketOption))
      
  ]

zooEntryTicketOption :: forall w. ST.TicketBookingScreenState -> Array  {title :: String, currentValue :: String}
zooEntryTicketOption state = [{
  title : "Adult Ticket (₹200 per person)",
  currentValue : "2" 
  },
  {
  title : "Child Ticket (₹50 per person)",
  currentValue : "2" 
  }]

aquariumEntryTicketOption :: forall w. ST.TicketBookingScreenState -> Array  {title :: String, currentValue :: String}
aquariumEntryTicketOption state = [{
  title : "Adult Ticket (20 per person)",
  currentValue : "2" 
  },
  {
  title : "Child Ticket (₹10 per person)",
  currentValue : "2" 
  }]

photoVideographyTicketOption :: forall w. ST.TicketBookingScreenState -> Array  {title :: String, currentValue :: String}
photoVideographyTicketOption state = [{
  title : "Devices (₹250 per person)",
  currentValue : "2" 
  }]

incrementDecrementView :: forall w. {title :: String, currentValue :: String} -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
incrementDecrementView config push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginTop 24
  ][  textView $
      [ text config.title 
      , color Color.black800
      , margin $ MarginBottom 8
      ] <> FontStyle.subHeading1 TypoGraphy
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , padding $ Padding 4 4 4 4
      , cornerRadius 8.0
      , background Color.white900
      , stroke $ "1," <> Color.grey900
      ][  textView $
          [ background Color.grey700
          , text "-"
          , gravity CENTER
          , cornerRadius 4.0
          , width WRAP_CONTENT
          , padding $ Padding 28 5 28 10
          , height WRAP_CONTENT
          ] <> FontStyle.subHeading1 TypoGraphy
        , textView $
          [ background Color.white900
          , text config.currentValue
          , height WRAP_CONTENT
          , color Color.black800
          , weight 1.0
          , gravity CENTER
          ] <> FontStyle.subHeading1 TypoGraphy
        , textView $
          [ background Color.black900
          , text "+"
          , color Color.yellow900
          , padding $ Padding 28 5 28 10
          , cornerRadius 4.0
          , width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity CENTER
          ] <> FontStyle.subHeading1 TypoGraphy

      ]

  ]

bookingConfirmationView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
bookingConfirmationView state push = 
  linearLayout[][]

ticketInfoView :: forall w. ST.TicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
ticketInfoView state push = 
  linearLayout[][]