module Screens.TicketBookingFlow.MetroMyTickets.View where

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
import Screens.TicketBookingFlow.MetroMyTickets.Controller
import Screens.Types as ST
import Styles.Colors as Color
import Data.Array as DA
import Font.Size as FontSize
import Mobility.Prelude
import Debug
import Helpers.Utils



screen :: ST.MetroMyTicketsScreenState -> Screen Action ST.MetroMyTicketsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "MetroTicketDetailsScreen"
  , globalEvents : []
  , eval :
    \action state -> do
        let _ = spy "MetroMyTicketsScreen action " action
        let _ = spy "MetroMyTicketsScreen state " state
        eval action state
  }

view :: forall w . (Action -> Effect Unit) -> ST.MetroMyTicketsScreenState -> PrestoDOM (Effect Unit) w
view push state = 
  linearLayout[
    width MATCH_PARENT
  , height MATCH_PARENT
  , background Color.white900
  , clickable true
  -- , onBackPressed push $ const BackPressed
  , orientation VERTICAL
  ][
    headerView push state
  , scrollableView push state
  ]

headerView :: forall w . (Action -> Effect Unit) -> ST.MetroMyTicketsScreenState -> PrestoDOM (Effect Unit) w
headerView push state = 
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , padding $ PaddingBottom 1
  , background Color.grey900
  ][
    linearLayout[
      width MATCH_PARENT
    , height WRAP_CONTENT
    , padding $ Padding 16 16 16 15
    , background Color.white900
    ][
      imageView [
        width $ V 24
      , height $ V 27
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      -- , onClick push $ const BackPressed
      ]
    , textView $ [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ "My Tickets"
      , color Color.black800
      , margin $ MarginLeft 8
      ] <> FontStyle.subHeading1 TypoGraphy 
    ]
  ]

scrollableView :: forall w . (Action -> Effect Unit) -> ST.MetroMyTicketsScreenState -> PrestoDOM (Effect Unit) w
scrollableView push state = 
  scrollView [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][
    linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ][
      activeTicketsListView push state
    , pastTicketsListView push state
    ]
  ]

activeTicketsListView :: forall w . (Action -> Effect Unit) -> ST.MetroMyTicketsScreenState -> PrestoDOM (Effect Unit) w
activeTicketsListView push state = 
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ Margin 16 24 16 0
  , orientation VERTICAL
  ][
    textView $ [
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , text "Active Tickets"
    , color Color.black900
    ] <> FontStyle.subHeading1 TypoGraphy
  , linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 13
    ] [
      activeTicketView push state
    ]
  ]

activeTicketView :: forall w . (Action -> Effect Unit) -> ST.MetroMyTicketsScreenState -> PrestoDOM (Effect Unit) w
activeTicketView push state = 
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT 
  , padding $ Padding 1 1 1 1
  , cornerRadius 8.0
  , orientation VERTICAL
  , background Color.grey900
  ][ 
    linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT 
    , padding $ Padding 16 16 16 16
    , cornerRadius 8.0
    , orientation VERTICAL
    , background Color.white900
    ][
      linearLayout [
        width MATCH_PARENT
      , height WRAP_CONTENT
      ][
        linearLayout [
          width WRAP_CONTENT
        , height MATCH_PARENT
        , gravity CENTER_VERTICAL
        ][
          imageView [
            width $ V 41
          , height $ V 41
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chennai_metro"
          ]
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
          , text "Anna Nagar East to Chennai International Airport"
          , color Color.black800
          ] <> FontStyle.body1 TypoGraphy
        , linearLayout [
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , margin $ MarginTop 3
          , gravity CENTER_VERTICAL
          ] [
            textView $ [
              width WRAP_CONTENT
            , height WRAP_CONTENT
            , text "10th Jan, 24"
            , color Color.black700
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
            , text $ "6 tickets"
            , color Color.black700
            ] <> FontStyle.tags TypoGraphy
          ]
        ]
      ]
    , linearLayout [
        width MATCH_PARENT
      , height $ V 1
      , margin $ MarginVertical 16 16
      , background Color.grey900
      ][]
    , linearLayout [
        width MATCH_PARENT
      , height WRAP_CONTENT
      ][
        linearLayout [
          width WRAP_CONTENT
        , height WRAP_CONTENT
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
          , text $ "Valid until " <> "10th Jan, 24"
          ] <> FontStyle.tags TypoGraphy
        ]
      , linearLayout [
          height WRAP_CONTENT
        , weight 1.0
        ][]
      , linearLayout [
          width WRAP_CONTENT
        , height MATCH_PARENT
        , gravity CENTER_VERTICAL 
        ][
          imageView [
            width $ V 16
          , height $ V 16
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_blue_share"
          ]
        , textView $ [
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , text "Share"
          , color Color.blue900
          , margin $ MarginLeft 8
          ] <> FontStyle.tags TypoGraphy
        ]
      ]
    ]
  ]

pastTicketsListView :: forall w . (Action -> Effect Unit) -> ST.MetroMyTicketsScreenState -> PrestoDOM (Effect Unit) w
pastTicketsListView push state = 
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ Margin 16 32 16 0
  , orientation VERTICAL
  ][
    textView $ [
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , text "Past Tickets"
    , color Color.black900
    ] <> FontStyle.subHeading1 TypoGraphy
  , linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 13
    , orientation VERTICAL
    ] [
      pastTicketView push state
    ]
  ]

pastTicketView :: forall w . (Action -> Effect Unit) -> ST.MetroMyTicketsScreenState -> PrestoDOM (Effect Unit) w
pastTicketView push state = 
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , padding $ Padding 1 1 1 1
  , cornerRadius 8.0
  , background Color.grey900
  , gravity CENTER
  ][
    linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , padding $ Padding 16 16 16 16
    , cornerRadius 8.0
    , background Color.white900
    ][
      linearLayout [
        width WRAP_CONTENT
      , height MATCH_PARENT
      , gravity CENTER_VERTICAL
      ][
        imageView [
          width $ V 30
        , height $ V 30
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chennai_metro"
        ]
      ]
    , linearLayout [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , width WRAP_CONTENT
      , margin $ MarginLeft 9
      , orientation VERTICAL
      ][
        textView $ [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , text "Anna Nagar East to Egmore"
        , color Color.black800
        ] <> FontStyle.body1 TypoGraphy
      , linearLayout [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , margin $ MarginTop 3
        , gravity CENTER_VERTICAL
        ] [
          textView $ [
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , text "10th Jan, 24"
          , color Color.black700
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
          , text $ "6 tickets"
          , color Color.black700
          ] <> FontStyle.tags TypoGraphy
        ]
      , textView $ [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ "Expired"
        , color Color.green900
        ] <> FontStyle.body3 TypoGraphy
      ]
    ]
  ]
  
 