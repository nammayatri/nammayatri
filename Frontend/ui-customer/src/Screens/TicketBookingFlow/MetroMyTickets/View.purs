{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
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
import Animation as Anim
import Language.Strings
import Language.Types
import Services.Backend as Remote
import Engineering.Helpers.Commons as EHC
import Services.API as API
import Engineering.Helpers.Utils as EHU
import Screens.TicketBookingFlow.MetroMyTickets.ComponentConfig as Config
import Components.PrimaryButton as PrimaryButton 
import Helpers.FrfsUtils (TicketStatus(..), getTicketStatus)

screen :: ST.MetroMyTicketsScreenState -> Screen Action ST.MetroMyTicketsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "MetroMyTickets"
  , globalEvents : [
      ( \push -> do
        void $ launchAff_ $ void $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ do 
          (API.GetMetroBookingListResp resp)<- Remote.getMetroBookingStatusListBT (show initialState.props.ticketServiceType) Nothing Nothing
          lift $ lift $ doAff do liftEffect $ push $ MetroBookingListRespAC resp
        pure $ pure unit
      )
    ]
  , eval :
    \action state -> do
        let _ = spy "MetroMyTickets action " action
        let _ = spy "MetroMyTickets state " state
        eval action state
  }

view :: forall w . (Action -> Effect Unit) -> ST.MetroMyTicketsScreenState -> PrestoDOM (Effect Unit) w
view push state = 
  let 
    etvVisibility = null state.data.activeTickets && null state.data.pastTickets
  in
  Anim.screenAnimation $ linearLayout[
    width MATCH_PARENT
  , height MATCH_PARENT
  , background Color.white900
  , clickable true
  , onBackPressed push $ const BackPressed
  , orientation VERTICAL
  , afterRender push $ const AfterRender
  ] [ headerView push state
    , emptyTicketsView state push (boolToVisibility $ etvVisibility && not state.props.showShimmer)
    , scrollableView push state (boolToVisibility$ not etvVisibility)
    , shimmerView state
    ]

shimmerView :: forall w . ST.MetroMyTicketsScreenState -> PrestoDOM (Effect Unit) w
shimmerView state =
  shimmerFrameLayout[ 
    width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , visibility $ boolToVisibility state.props.showShimmer
  ][linearLayout[
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin (MarginTop 15)
    ] (DA.mapWithIndex 
        (\index item ->
            linearLayout
              [ width MATCH_PARENT
              , height (V 90)
              , margin (Margin 16 16 16 0)
              , cornerRadius 12.0
              , background Color.greyDark
              ][]
        ) (1 .. 4)
      )
    ]

emptyTicketsView :: forall w . ST.MetroMyTicketsScreenState -> (Action -> Effect Unit) -> Visibility -> PrestoDOM (Effect Unit) w
emptyTicketsView state push etvVisibility = 
  linearLayout[
    height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  , visibility etvVisibility
  ][
    linearLayout[ 
      width MATCH_PARENT
    , height WRAP_CONTENT
    , weight 1.0
    , gravity CENTER
    , orientation VERTICAL
    , padding $ PaddingHorizontal 20 20
    ]
    [ imageView
      [ width $ V 128
      , height $ V 128
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_ticket_grey"
      ]
    , textView $
      [ width $ V 188
      , height WRAP_CONTENT
      , text $ getString YOUR_BOOKED_TICKETS
      , color Color.black900
      , gravity CENTER
      , margin $ MarginTop 5
      ] <> FontStyle.paragraphText TypoGraphy
    ]
    , bookTicketsButtonView state push
  ]

bookTicketsButtonView :: forall w. ST.MetroMyTicketsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
bookTicketsButtonView state push = 
  linearLayout
  [ orientation VERTICAL
  , height WRAP_CONTENT
  , width MATCH_PARENT
  , background Color.white900
  , padding $ PaddingVertical 5 24
  , alignParentBottom "true,-1"
  , visibility $ boolToVisibility (not state.data.userBlocked)
  ][  linearLayout
      [ height $ V 1
      , width MATCH_PARENT
      , background Color.greySmoke
      , margin $ MarginBottom 16
      ][]
    , PrimaryButton.view (push <<< GoToMetroBookingScreen) Config.bookTicketsButtonConfig]

headerView :: forall w . (Action -> Effect Unit) -> ST.MetroMyTicketsScreenState -> PrestoDOM (Effect Unit) w
headerView push state =
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , padding $ PaddingBottom 1
  , background Color.grey900
  , margin $ MarginTop safeMarginTop
  ][
    linearLayout[
      width MATCH_PARENT
    , height WRAP_CONTENT
    , padding $ Padding 16 16 16 15
    , background Color.white900
    , gravity CENTER_VERTICAL
    ][
      imageView [
        width $ V 24
      , height $ V 27
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , onClick push $ const BackPressed
      , accessibility ENABLE 
      , accessibilityHint "Go Back Button"
      ]
    , textView $ [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ getString MY_TICKETS
      , color Color.black800
      , margin $ MarginLeft 8
      ] <> FontStyle.subHeading1 TypoGraphy 
    ]
  ]

scrollableView :: forall w . (Action -> Effect Unit) -> ST.MetroMyTicketsScreenState -> Visibility -> PrestoDOM (Effect Unit) w
scrollableView push state svVisibility = 
  scrollView [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , visibility svVisibility
  ][
    linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ] [
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
  , visibility $ if not $ null state.data.activeTickets then VISIBLE else GONE
  ][
    textView $ [
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , text $ getString ACTIVE_TICKETS
    , color Color.black900
    ] <> FontStyle.subHeading1 TypoGraphy
  , linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 13
    ] $ map (\ ticket -> activeTicketView push ticket (show state.props.ticketServiceType)) state.data.activeTickets
  ]

activeTicketView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketCardData -> String -> PrestoDOM (Effect Unit) w
activeTicketView push ticketCard vehicleType= 
  let
    isStatusPending = ticketCard.status == "PAYMENT_PENDING"
  in
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT 
  , padding $ Padding 1 1 1 1
  , cornerRadius 8.0
  , orientation VERTICAL
  , background Color.grey900
  , margin $ MarginBottom 16
  , onClick push $ const $ TicketPressed ticketCard.metroTicketStatusApiResp
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
      , gravity CENTER_VERTICAL
      ][
        linearLayout [
          width WRAP_CONTENT
        , height $ V 41
        , gravity CENTER_VERTICAL
        ][
          imageView [
            width $ V 41
          , height $ V 41
          , imageWithFallback $ fetchImage FF_COMMON_ASSET (Config.getMetroLogoImage ticketCard vehicleType)
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
          , text $ ticketCard.sourceName <> " to " <> ticketCard.destinationName
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
            , text ticketCard.createdAt
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
            , text $ (show ticketCard.noOfTickets) <> " " <> if ticketCard.noOfTickets > 1 then (getString TICKETS) else (getString TICKET)
            , color Color.black700
            ] <> FontStyle.tags TypoGraphy
          ]
          , statusView ticketCard $ boolToVisibility isStatusPending
        ]
      ]
    , linearLayout [
        width MATCH_PARENT
      , height $ V 1
      , margin $ MarginVertical 16 16
      , background Color.grey900
      , visibility $ boolToVisibility $ not isStatusPending
      ][]
    , linearLayout [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , visibility $ boolToVisibility $ not isStatusPending
      ][
        linearLayout [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER
        ][
          imageView [
            width $ V 16
          , height $ V 16
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ic_clock_filled"
          , margin $ MarginRight 4
          ] 
        , textView $ [
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ (getString VALID_UNTIL) <> " " <> ticketCard.validUntill
          ] <> FontStyle.tags TypoGraphy
        ]
      -- , linearLayout [
      --     height WRAP_CONTENT
      --   , weight 1.0
      --   ][]
      -- , linearLayout [
      --     width WRAP_CONTENT
      --   , height MATCH_PARENT
      --   , gravity CENTER_VERTICAL 
      --   ][
      --     imageView [
      --       width $ V 16
      --     , height $ V 16
      --     , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_blue_share"
      --     ]
      --   , textView $ [
      --       width WRAP_CONTENT
      --     , height WRAP_CONTENT
      --     , text "Share"
      --     , color Color.blue900
      --     , margin $ MarginLeft 8
      --     ] <> FontStyle.tags TypoGraphy
      --   ]
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
  , visibility $ if not $ null state.data.pastTickets then VISIBLE else GONE
  ][
    textView $ [
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , text $ getString PAST_TICKETS
    , color Color.black900
    ] <> FontStyle.subHeading1 TypoGraphy
  , linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 13
    , orientation VERTICAL
    ] $ map (\ticket ->  pastTicketView push ticket (show state.props.ticketServiceType)) (reverse state.data.pastTickets)
  ]

pastTicketView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketCardData ->String-> PrestoDOM (Effect Unit) w
pastTicketView push ticketCard vehicleType= 
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , padding $ Padding 1 1 1 1
  , cornerRadius 8.0
  , margin $ MarginBottom 16
  , background Color.grey900
  , gravity CENTER
  , onClick push $ const $ TicketPressed ticketCard.metroTicketStatusApiResp
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
        , imageWithFallback $ fetchImage FF_COMMON_ASSET (Config.getMetroLogoImage ticketCard vehicleType)
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
        , text $ ticketCard.sourceName <> " to " <> ticketCard.destinationName
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
          , text ticketCard.createdAt
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
          , text $ (show ticketCard.noOfTickets) <> " " <> (getString TICKETS)
          , color Color.black700
          ] <> FontStyle.tags TypoGraphy
        ]
      , statusView ticketCard VISIBLE
    ]
  ]
]

statusView :: forall w . ST.MetroTicketCardData -> Visibility -> PrestoDOM (Effect Unit) w
statusView ticketCard statusVisibility = 
  let
    (TicketStatus ticketStatus) = getTicketStatus ticketCard
  in
    linearLayout [
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , margin $ MarginTop 6
    , visibility $ statusVisibility
    ][
      imageView [
        width $ V 16
      , height $ V 16
      , imageWithFallback ticketStatus.statusIcon
      , margin $ MarginRight 4
      ]
    , textView $ [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ (show ticketStatus.status)
      , color ticketStatus.statusColor
      ] <> FontStyle.body3 TypoGraphy
    ]