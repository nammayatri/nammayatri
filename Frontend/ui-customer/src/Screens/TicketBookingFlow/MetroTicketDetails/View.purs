module Screens.TicketBookingFlow.MetroTicketDetails.View where

import Prelude


import Prelude
import Common.Types.App (LazyCheck(..))

import Data.Array 
import Presto.Core.Types.Language.Flow 
import Effect.Aff 
import Types.App
import Control.Monad.Except.Trans 
import Control.Transformers.Back.Trans 
import Engineering.Helpers.Commons 
import Effect.Class 
import Data.Maybe 
import Effect 
import Font.Style as FontStyle
import PrestoDOM 
import Screens.TicketBookingFlow.MetroTicketDetails.Controller
import Screens.Types as ST
import Styles.Colors as Color
import Data.Array as DA
import Font.Size as FontSize
import Mobility.Prelude
import Debug
import Helpers.Utils
import Effect.Uncurried
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim

screen :: ST.MetroTicketDetailsScreenState -> Screen Action ST.MetroTicketDetailsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "MetroTicketDetailsScreen"
  , globalEvents : []
  , eval :
    \action state -> do
        let _ = spy "MetroTicketDetailsScreen action " action
        let _ = spy "MetroTicketDetailsScreen state " state
        eval action state
  }


view :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
view push state =  
  let 
    bodyView = case state.props.stage of 
                  ST.MetroTicketDetailsStage -> metroTicketDetailsView
                  ST.MetroMapStage -> mapView
                  ST.MetroRouteDetailsStage -> routeDetailsView
  in
    Anim.screenAnimation $ linearLayout[
      width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.grey700
    , clickable true
    , onBackPressed push $ const BackPressed
    , orientation VERTICAL
    ][
      headerView push state
    , bodyView push state
    ] 

headerView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
headerView push state = 
  let 
    headerText = case state.props.stage of 
                  ST.MetroTicketDetailsStage -> "Ticket Details"
                  ST.MetroMapStage -> "Map"
                  ST.MetroRouteDetailsStage -> "Route Details"
    shareButtonVisibility =  boolToVisibility $ state.props.stage == ST.MetroTicketDetailsStage
  in 
    linearLayout[
      width MATCH_PARENT
    , height WRAP_CONTENT
    , padding $ Padding 16 16 16 16 
    , background Color.white900
    ][
      imageView [
        width $ V 24
      , height $ V 27
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , onClick push $ const BackPressed
      ]
    , textView $ [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , text headerText
      , color Color.black800
      , margin $ MarginLeft 8
      ] <> FontStyle.subHeading1 TypoGraphy 
    , linearLayout [
        height WRAP_CONTENT
      , weight 1.0
      ][]
    , linearLayout [
        width WRAP_CONTENT
      , height MATCH_PARENT
      , onClick push $ const ShareTicketClick
      , gravity CENTER_VERTICAL 
      , visibility shareButtonVisibility
      ][
        imageView [
          width $ V 16
        , height $ V 16
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_blue_share"
        ]
      , textView $ [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , text "Share Ticket"
        , color Color.blue900
        , margin $ MarginLeft 8
        ] <> FontStyle.tags TypoGraphy
      ]
    ]

metroTicketDetailsView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
metroTicketDetailsView push state = 
  scrollView [
    width MATCH_PARENT
  , height WRAP_CONTENT
  ][
    linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , id $ getNewIDWithTag "MetroTicketView"
    ][
      ticketDetailsView push state
    , paymentInfoView push state
    ]
  ]

ticketDetailsView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
ticketDetailsView push state = 
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , gravity CENTER
  , id $ getNewIDWithTag "metro_ticket_details_view"
  ][
    linearLayout[
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding $ Padding 16 30 16 16
    , margin $ Margin 16 24 16 0
    , background Color.white900
    , cornerRadius 8.0
    ][
      metroHeaderView push state
    , qrCodeView push state
    , ticketNumberAndValidView push state
    ]
  , linearLayout[
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.white900
    , padding $ Padding 16 30 16 16
    , margin $ MarginHorizontal 16 16 
    , cornerRadius 8.0
    ][
      originAndDestinationView push state
    ]
  ]

metroHeaderView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
metroHeaderView push state = 
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT
  ][
    imageView [
      width $ V 41
    , height $ V 41
    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chennai_metro"
    ]
  , linearLayout [
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , width WRAP_CONTENT
    , margin $ MarginLeft 10
    , orientation VERTICAL
    ][
      textView $ [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , text "Tickets for Chennai Metro"
      , color Color.black800
      ] <> FontStyle.body20 TypoGraphy
    , linearLayout [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , margin $ MarginTop 3
      , gravity CENTER_VERTICAL
      ] [
        textView $ [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , text "Onward Journey"
        , color Color.black800
        ] <> FontStyle.tags TypoGraphy
      , linearLayout [
          width $ V 4
        , height $ V 4
        , cornerRadius 2.0
        , margin $ MarginHorizontal 6 6
        , background Color.black500
        , gravity CENTER_VERTICAL
        ][]
      , textView $ [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ (show $ state.props.currentTicketIndex + 1) <> " tickets"
        , color Color.black800
        ] <> FontStyle.tags TypoGraphy
      ]
    ]
  ]

qrCodeView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
qrCodeView push state = 
  let 
    currentTicket = state.data.ticketsInfo !! state.props.currentTicketIndex
    qrString = case currentTicket of 
                Just ticket -> ticket.qrString
                Nothing -> ""
  in 
    linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop 20
    , orientation VERTICAL
    , gravity CENTER
    , visibility $ boolToVisibility $ isJust currentTicket
    ][
      textView $ [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ (show $ state.props.currentTicketIndex + 1) <> "/" <> (show $ length state.data.ticketsInfo) <>" Tickets"
      , color Color.black800
      , gravity CENTER
      ] <> FontStyle.subHeading1 TypoGraphy
    , linearLayout[
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER_VERTICAL
      ][
        imageView [
          width $ V 32
        , height $ V 32
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left_grey"
        , onClick push $ const PrevTicketClick
        ]
      , linearLayout [
          height WRAP_CONTENT
        , weight 1.0
        ][]
      , PrestoAnim.animationSet [ Anim.fadeInWithDelay 50 true ] $ imageView [
          width $ V 218
        , height $ V 218
        , id $ getNewIDWithTag "metro_ticket_qr_code"
        , onAnimationEnd push (const (TicketQRRendered (getNewIDWithTag "metro_ticket_qr_code") qrString))
        ]
      , linearLayout [
          height WRAP_CONTENT
        , weight 1.0
        ][]
      , imageView [
          width $ V 32
        , height $ V 32
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right_grey"
        , onClick push $ const NextTicketClick
        ]
      ]
    ]

ticketNumberAndValidView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
ticketNumberAndValidView push state = 
  let 
    currentTicket = state.data.ticketsInfo !! state.props.currentTicketIndex
    ticketNumber = case currentTicket of 
                    Just ticket -> ticket.ticketNumber
                    Nothing -> ""
    validUntil = case currentTicket of 
                    Just ticket -> ticket.validUntil
                    Nothing -> ""
  in 
    linearLayout[
      width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop 20
    , orientation VERTICAL
    , gravity CENTER
    ][
      linearLayout [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , padding $ Padding 12 8 12 8
      , background Color.blue600 
      , cornerRadius 51.0
      , gravity CENTER
      ][
        textView $ [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ "Ticket Number:" <> ticketNumber
        , color Color.black800
        ] <> FontStyle.body20 TypoGraphy
      ]
    , linearLayout [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , margin $ MarginTop 10
      , gravity CENTER
      ][
        imageView [
          width $ V 16
        , height $ V 16
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_yellow_clock"
        , margin $ MarginRight 4
        ] 
      , textView $ [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ "Valid until " <> validUntil
        ] <> FontStyle.tags TypoGraphy
      ]
    ]

originAndDestinationView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
originAndDestinationView push state = 
  let originStation = state.data.metroRoute !! 0
      destinationStation = state.data.metroRoute !! ((length state.data.metroRoute) - 1)
      originConfig = case originStation of
        Nothing -> {
          name : ""
        , line : ST.NoColorLine
        }
        Just station -> {
          name : station.name
        , line : station.line
        }
      destinationConfig = case destinationStation of
        Nothing -> {
          name : ""
        , line : ST.NoColorLine
        }
        Just station -> {
          name : station.name
        , line : station.line
        }
  in
    linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , gravity CENTER_VERTICAL
    , cornerRadius 8.0
    ][
      linearLayout [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][
        linearLayout [
          width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        ][
          linearLayout [
            width $ V 8
          , height $ V 8
          , cornerRadius 4.0
          , background Color.green900
          ][]
        , textView $ [
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , text "Origin"
          , color Color.black700
          , margin $ MarginLeft 6
          ] <> FontStyle.body3 TypoGraphy
        ]
      , linearLayout [
          width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        ][
          textView $ [
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , text originConfig.name
          ] <> FontStyle.body1 TypoGraphy
        , linePillView originConfig.line
        ]
      ]
    , linearLayout [
        width MATCH_PARENT
      , height $ V 1
      , background $ Color.ghostWhite
      , margin $ MarginVertical 12 12
      ][]
    , linearLayout [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][
        linearLayout [
          width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        ][
          linearLayout [
            width $ V 8
          , height $ V 8
          , cornerRadius 4.0
          , background Color.red900
          ][]
        , textView $ [
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , text "Destination"
          , color Color.black700
          , margin $ MarginLeft 6
          ] <> FontStyle.body3 TypoGraphy
        ]
      , linearLayout [
          width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        ][
          textView $ [
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , text destinationConfig.name
          ] <> FontStyle.body1 TypoGraphy
        , linePillView destinationConfig.line
        ]
      ]
    ]

paymentInfoView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
paymentInfoView push state = 
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , padding $ Padding 16 20 16 20
  , margin $ Margin 16 48 16 16
  , onClick push $ const ViewPaymentInfoClick
  , background Color.white900
  , cornerRadius 8.0
  ][
    textView $ [
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , text "View Route Information"
    ] <> FontStyle.body1 TypoGraphy
  , linearLayout [
      height WRAP_CONTENT
    , weight 1.0
    ][]
  , imageView [
      width $ V 20
    , height $ V 20
    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_down"
    ]
  ]

linePillView :: forall w . ST.MetroLine -> PrestoDOM (Effect Unit) w
linePillView line = 
  let 
    pillConfig = case line of 
      ST.GreenLine -> {text : "Green Line", color : Color.green900, bg : Color.tealishGreen}
      ST.BlueLine -> {text : "Blue Line", color : Color.blue900, bg : Color.blue600}
      ST.RedLine -> {text : "Red Line", color : Color.red900, bg : Color.red600}
      _ -> {text : "", color : Color.black800, bg : Color.black600}
  in 
    linearLayout [
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , padding $ Padding 8 2 8 2
    , background pillConfig.bg
    , cornerRadius 4.0 
    , margin $ MarginLeft 8
    ][
      textView $ [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , text pillConfig.text
      , color pillConfig.color
      , padding $ PaddingBottom 4
      ] <> FontStyle.body15 TypoGraphy
    ]


mapView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
mapView push state = 
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity START 
  ][
    imageView [
      width MATCH_PARENT
    , height $ V (screenWidth unit)
    , margin $ MarginTop 24
    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chennai_metro_map"
    ]
  ]

routeDetailsView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
routeDetailsView push state = 
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ Margin 16 24 16 0
  , padding $ Padding 16 16 16 16
  , background Color.white900 
  , cornerRadius 8.0
  , orientation VERTICAL
  ] $ mapWithIndex (\index route -> routeDetailsItemView push index route) state.data.metroRoute

routeDetailsItemView :: forall w . (Action -> Effect Unit) -> Int -> ST.MetroRoute -> PrestoDOM (Effect Unit) w
routeDetailsItemView push index routeDetails = 
  let 
    noOfStops = length routeDetails.stops
    pillText = case routeDetails.line of 
                ST.GreenLine -> "Green Line"
                ST.BlueLine -> "Blue Line"
                ST.RedLine -> "Red Line"
                _ -> ""
  in
    linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL 
    , margin $ MarginVertical 4 4
    ][
      linearLayout [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      ][
        linearLayout [
          width $ V 20
        , height $ V 20
        , background metroPrimaryColor
        , cornerRadius 10.0
        , gravity CENTER
        ][
          imageView [
            width $ V 9
          , height $ V 12
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_metro_logo_mini"
          ]
        ]
      , textView $ [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , text routeDetails.name
        , margin $ MarginLeft 8
        ] <> FontStyle.body1 TypoGraphy
      ]
    , linearLayout [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      ][
        linearLayout [
          width $ V 2
        , height MATCH_PARENT
        , margin $ Margin 9 8 9 0
        , background Color.grey900
        , visibility if noOfStops > 0 then VISIBLE else INVISIBLE
        ][]
      , linearLayout [
          width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ] $ [
          linearLayout [
            width MATCH_PARENT
          , height WRAP_CONTENT
          , margin $ MarginTop 8
          , visibility $ boolToVisibility $ noOfStops > 0
          ][
            linePillView routeDetails.line
          , linearLayout [
              height WRAP_CONTENT
            , width WRAP_CONTENT
            , padding $ Padding 8 2 8 2
            , background pillBG
            , cornerRadius 4.0
            , margin $ MarginLeft 8 
            , onClick push $ const $ StopsBtnClick index
            , gravity CENTER
            ][
              textView $ [
                width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ (show noOfStops) <> " Stops"
              , padding $ PaddingBottom 4
              , color metroPrimaryColor
              ] <> FontStyle.tags TypoGraphy
              , imageView [
                width $ V 24
              , height $ V 15
              , padding $ PaddingLeft 8
              , imageWithFallback $ fetchImage FF_COMMON_ASSET (if routeDetails.listExpanded then "ny_ic_chevron_up" else "ny_ic_chevron_down" )
              ]
            ]
          ]
        ] <> if routeDetails.listExpanded then [stopsListView] else [] 
      ]
    ]
  where 
    metroPrimaryColor = case routeDetails.line of 
                      ST.GreenLine -> Color.green900
                      ST.BlueLine -> Color.blue800 
                      ST.RedLine -> Color.red900
                      _ -> Color.black800

    pillBG = case routeDetails.line of 
                ST.GreenLine -> Color.tealishGreen
                ST.BlueLine -> Color.blue600 
                ST.RedLine -> Color.red600
                _ -> Color.black600

    stopsListView :: forall w . PrestoDOM (Effect Unit) w
    stopsListView = 
      linearLayout [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , margin $ MarginTop 8
      ] $ mapWithIndex (\index stop -> stopItemView index stop) routeDetails.stops
    
    stopItemView :: forall w . Int -> ST.MetroStop -> PrestoDOM (Effect Unit) w
    stopItemView index stop = 
      linearLayout [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , margin $ MarginTop 8
      , gravity CENTER_VERTICAL
      ][
        linearLayout [
          width $ V 16
        , height $ V 16
        , cornerRadius 8.0
        , background pillBG
        , gravity CENTER
        ][ 
          textView $ [
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ show $ index + 1
          , color metroPrimaryColor
          , padding $ PaddingBottom 4
          ] <> FontStyle.body16 TypoGraphy
        ]
      , textView $ [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , text stop.name
        , padding $ PaddingBottom 4
        , margin $ MarginLeft 8
        ] <> FontStyle.body1 TypoGraphy
      ]