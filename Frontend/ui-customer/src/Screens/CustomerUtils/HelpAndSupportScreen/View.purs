{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HelpAndSupportScreen.View where

import Animation as Anim
import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.SourceToDestination as SourceToDestination
import Data.Array as DA
import Data.Either (Either(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC 
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB 
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, discard, map, pure, unit, ($), (-), (/=), (<<<), (<=), (<>), (==), (||))
import Presto.Core.Types.Language.Flow (Flow, doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Shadow(..), Visibility(..), afterRender, alignParentRight, background, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, shadow, stroke, text, textSize, textView, visibility, width, imageWithFallback)
import PrestoDOM.Properties as PP
import PrestoDOM.Types.DomAttributes as PTD
import Screens.HelpAndSupportScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Services.API (RideBookingListRes(..))
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (GlobalState)
import Common.Types.App
import Screens.CustomerUtils.HelpAndSupportScreen.ComponentConfig
import Constant.Test as Id
import EN

screen :: ST.HelpAndSupportScreenState -> Screen Action ST.HelpAndSupportScreenState ScreenOutput
screen initialState = 
  {
    initialState
  , view
  , name : "HelpAndSupportScreen"
  , globalEvents : [
      (\push -> do 
                  if (initialState.data.source == "") then do
                    launchAff_ $ EHC.flowRunner $ getPastRides RideBookingListAPIResponseAction push initialState
                    else pure unit
                  pure $ pure unit
      )
  ]
  , eval : \state  action -> do
      let _ = spy  "HelpAndSupportScreen action " state
      let _ = spy  "HelpAndSupportScreen state " action
      eval state action
  }

view :: forall w . (Action -> Effect Unit) -> ST.HelpAndSupportScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $ 
 relativeLayout
 [  height MATCH_PARENT
  , width MATCH_PARENT
 ][ linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , padding $ Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom
    , onBackPressed push $ const BackPressed state.props.isCallConfirmation
    , afterRender push (const AfterRender)
    , Id.testId $ Id.Screen Id.helpAndSupportScreen
    ][  GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , visibility if state.props.apiFailure || state.data.isNull then GONE else VISIBLE
        , background Color.catskillWhite
        ][  textView
            [ text (getString YOUR_RECENT_RIDE)
            , textSize FontSize.a_16
            , color Color.darkDescriptionText
            , width WRAP_CONTENT
            , fontStyle $ FontStyle.medium LanguageStyle
            , margin (Margin 16 12 0 12)
            ]
          , textView
            [ text (getString VIEW_ALL_RIDES)
            , alignParentRight "true,-1"
            , textSize FontSize.a_14
            , margin (Margin 0 14 16 14)
            , width MATCH_PARENT
            , gravity RIGHT
            , fontStyle $ FontStyle.medium LanguageStyle
            , color Color.blue900
            , onClick push $ const ViewRides
            , Id.testId $ Id.Text (getEN VIEW_ALL_RIDES)
            ]
          ]
      , recentRideView state push
      , headingView state (getString ALL_TOPICS)
      , allTopicsView state push
      , apiFailureView state push 
      ]
    , if state.props.isCallConfirmation 
        then PopUpModal.view (push <<< PopupModelActionController) (callConfirmationPopup state)
        else 
          linearLayout [][]
  ] 

------------------------------- recentRide --------------------------
recentRideView :: ST.HelpAndSupportScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
recentRideView state push=
  linearLayout
  [ margin (Margin 16 16 16 16)
  , background Color.white900
  , cornerRadius 8.0
  , width MATCH_PARENT
  , orientation VERTICAL
  , stroke ("1," <> Color.greyLight)
  , height $ V 174
  , visibility if state.data.isNull || state.props.apiFailure then GONE else VISIBLE
  , shadow (Shadow 10.0 10.0 4.0 1.0 Color.black 0.9)
  ][
    linearLayout
    [ height $ V 134
    , width MATCH_PARENT
    , orientation HORIZONTAL
    ][  imageView
        [ background Color.greyLight
        , imageWithFallback "ny_ic_help_map,https://assets.juspay.in/nammayatri/images/user/ny_ic_help_map.png"
        , PP.cornerRadii $ PTD.Corners 8.0 true false false false 
        , height MATCH_PARENT
        , width $ V 130
        ]
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , margin (MarginLeft 12)
        ][  dateAndTimeView state 
          , SourceToDestination.view (push <<< SourceToDestinationActionController) (sourceToDestinationConfig state)
          , driverRatingView state 
          ]
        ]
  , linearLayout
    [ width MATCH_PARENT
    , height $ V 1
    , background Color.greyLight
    ][]
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , padding (Padding 10 10 10 10)
    , orientation HORIZONTAL
    , onClick push $ const ReportIssue
    , Id.testId $ Id.Container (getEN REPORT_AN_ISSUE_WITH_THIS_TRIP)
    ][  
    textView
        [ text (getString REPORT_AN_ISSUE_WITH_THIS_TRIP)
        , textSize FontSize.a_13
        , color Color.blue900
        , fontStyle $ FontStyle.medium LanguageStyle
        ]
     ,  linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity RIGHT
        ][  imageView
            [ imageWithFallback "ny_ic_chevron_right,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_right.png"
            , height $ V 15
            , width WRAP_CONTENT
            ]
          ] 
      ]
    ]
  
------------------------------- dateAndTimeView --------------------------
dateAndTimeView :: ST.HelpAndSupportScreenState -> forall w . PrestoDOM (Effect Unit) w
dateAndTimeView state =
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , orientation HORIZONTAL
  , margin (Margin 0 12 0 0)
  , gravity CENTER_VERTICAL
  ][  textView
      [ text state.data.date
      , textSize FontSize.a_11
      , color Color.greyShade
      , fontStyle $ FontStyle.medium LanguageStyle
      ]
    , linearLayout
      [ height MATCH_PARENT
      , width WRAP_CONTENT
      , gravity CENTER
      , orientation VERTICAL
      ][  linearLayout
          [ background Color.greyishBlue
          , cornerRadius 2.5
          , margin (Margin 5 3 5 0)
          , height (V 5)
          , width (V 5)
          ][]
       ]
    , textView
      [ text state.data.time
      , textSize FontSize.a_11
      , color Color.greyShade
      , fontStyle $ FontStyle.medium LanguageStyle
      ]
    ] 

------------------------------- driverRating --------------------------
driverRatingView :: ST.HelpAndSupportScreenState -> forall w . PrestoDOM (Effect Unit) w
driverRatingView state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , orientation HORIZONTAL
  , margin (Margin 0 13 0 10)
  ][  textView
      [ text (getString YOU_RATED)
      , textSize FontSize.a_12
      , color Color.darkDescriptionText
      , fontStyle $ FontStyle.regular LanguageStyle
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , padding (Padding 0 0 0 0)
      , margin (MarginLeft 4)
      , gravity LEFT
      ](map (\ item -> 
                        linearLayout
                        [ height WRAP_CONTENT
                        , width WRAP_CONTENT
                        , margin if (item /= 5) then (Margin 0 0 4 0) else (Margin 0 0 0 0)
                        ][imageView
                            [ height $ V 14
                            , width $ V 14
                            , imageWithFallback if item <= state.data.rating then "ny_ic_star_active,https://assets.juspay.in/nammayatri/images/common/ny_ic_star_active.png" else "ny_ic_star_inactive,https://assets.juspay.in/nammayatri/images/common/ny_ic_star_inactive.png"
                            ]
                          ]) [1 ,2 ,3 ,4 ,5])
    ]

------------------------------- allTopics --------------------------
allTopicsView :: ST.HelpAndSupportScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
allTopicsView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , visibility if state.props.apiFailure then GONE else VISIBLE
    , orientation VERTICAL
    ](DA.mapWithIndex (\index item -> 
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding (Padding 20 0 20 0)
        , onClick push (const item.action)
        , Id.testId $ Id.List if (item.title == (getString FOR_OTHER_ISSUES_WRITE_TO_US)) then (getEN FOR_OTHER_ISSUES_WRITE_TO_US) else (getString CONTACT_SUPPORT)
        , orientation VERTICAL
        ][  linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            , padding (Padding 0 20 0 20)
            ][  imageView
                [ imageWithFallback item.image
                , height $ V 17
                , width $ V 17
                ]
              , textView
                [ text item.title
                , textSize FontSize.a_14
                , color Color.darkDescriptionText
                , margin (MarginLeft 13)
                , fontStyle $ FontStyle.regular LanguageStyle
                ]
              , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity RIGHT
                ][  imageView
                    [ imageWithFallback "ny_ic_chevron_right,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_right.png"
                    , height $ V 15
                    , width WRAP_CONTENT
                    ]
                  ]
              ]
            , linearLayout
              [ height $ V 1
              , width MATCH_PARENT
              , background Color.greyLight
              , visibility if index == (DA.length (topicsList state)) - 1 then GONE else VISIBLE
              ][]
          ]) (topicsList state))

headingView :: ST.HelpAndSupportScreenState -> String -> forall w . PrestoDOM (Effect Unit) w
headingView state title = 
  textView
    [ text title
    , width MATCH_PARENT
    , visibility if state.props.apiFailure then GONE else VISIBLE
    , height WRAP_CONTENT
    , padding (Padding 16 12 0 12)
    , background Color.catskillWhite
    , textSize FontSize.a_16
    , color Color.darkDescriptionText
    ]

getPastRides :: forall action.( RideBookingListRes -> String -> action) -> (action -> Effect Unit) -> ST.HelpAndSupportScreenState ->  Flow GlobalState Unit
getPastRides action push state = do
  _ <-  JB.loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS) 
  _ <-  JB.toggleLoader true
  (rideBookingListResponse) <- Remote.rideBookingList "8" "0" "false"
  
  case rideBookingListResponse of 
      Right (RideBookingListRes  listResp) -> do 
          doAff do liftEffect $ push $ action (RideBookingListRes listResp) "success"
          _ <-  JB.toggleLoader false
          pure unit 
      Left (err) -> do 
        doAff do liftEffect $ push $ action (RideBookingListRes dummyListResp ) "failure"
        _ <-  JB.toggleLoader false
        pure unit

dummyListResp :: forall t127.
  { list :: Array t127
  }
dummyListResp = {list : []}

apiFailureView :: forall w. ST.HelpAndSupportScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit ) w  
apiFailureView state push= 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , gravity CENTER
  , visibility if state.props.apiFailure then VISIBLE else GONE
  ][  ErrorModal.view (push <<< APIFailureActionController) (apiErrorModalConfig state)]

topicsList :: ST.HelpAndSupportScreenState ->  Array { action :: Action
  , title :: String
  , image :: String
}
topicsList state = [
  { action : ContactUs
  , title : (getString FOR_OTHER_ISSUES_WRITE_TO_US)
  , image : "ny_ic_clip_board,https://assets.juspay.in/nammayatri/images/common/ny_ic_clip_board.png"
  },
  { action : CallSupport
  , title : (getString CONTACT_SUPPORT)
  , image : "ny_ic_help,https://assets.juspay.in/nammayatri/images/user/ny_ic_help.png"
  }
]