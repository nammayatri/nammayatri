{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TripDetailsScreen.View where

import Animation as Anim
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Language.Types (STR(..))
import Language.Strings (getString)
import Prelude (Unit, const, not, show, unit, ($), (*), (/), (<<<), (<>), (==), (&&))
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM (Length(..), Margin(..), Orientation(..), Padding(..), Gravity(..), Visibility(..), PrestoDOM, Screen, linearLayout, frameLayout, gravity, orientation, height, width, imageView, imageUrl, text, textSize, textView, padding, color, margin, fontStyle, background, cornerRadius, stroke, editText, weight, hint, onClick, visibility, pattern, onChange, scrollView, alignParentBottom, relativeLayout, afterRender, onBackPressed, imageWithFallback)
import Screens.Types as ST
import Engineering.Helpers.Commons as EHC
import Screens.TripDetailsScreen.Controller (Action(..), ScreenOutput, eval)
import Font.Size as FontSize
import Font.Style as FontStyle
import Styles.Colors as Color
import Components.GenericHeader as GenericHeader
import Components.SourceToDestination as SourceToDestination
import Common.Types.App
import Screens.TripDetailsScreen.ComponentConfig
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))
import MerchantConfig.Utils (Merchant(..), getMerchant, getValueFromConfig)

screen :: ST.TripDetailsScreenState -> Screen Action ST.TripDetailsScreenState ScreenOutput 
screen initialState = 
  { initialState
  , view
  , name : "TripDetailsScreen"
  , globalEvents : []
  , eval
  }

view
  :: forall w 
  . (Action -> Effect Unit) -> ST.TripDetailsScreenState -> PrestoDOM (Effect Unit) w 
view push state =
  Anim.screenAnimation $
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  , onBackPressed push (const BackPressed)
  , afterRender push (const AfterRender)
  ][linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][  GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
        , scrollView
          [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
          ][ 
            linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , orientation VERTICAL
              ][ 
                scrollView
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , visibility if state.props.issueReported then GONE else VISIBLE
                  ][  linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , orientation VERTICAL
                      , background Color.blue600
                      , padding (Padding 16 16 16 16)
                      ][ tripDetailsView state
                      , separatorView 
                      , tripDataView push state
                      , separatorView 
                      , SourceToDestination.view (push <<< SourceToDestinationActionController) (sourceToDestinationConfig state)
                      ]
                    ]
                , linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , visibility if state.props.issueReported then GONE else VISIBLE
                ][  linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , padding (Padding 16 16 16 16)
                    , orientation VERTICAL
                    ][ reportIssueView state push
                      , linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation VERTICAL
                    , padding $ PaddingVertical 10 10
                    ][ linearLayout
                       [ height WRAP_CONTENT
                       , width MATCH_PARENT
                       , orientation HORIZONTAL
                       , onClick push (const HelpAndSupport)
                        ][ imageView
                          [ imageWithFallback $ "ny_ic_support," <> (getAssetStoreLink FunctionCall) <> "ny_ic_support.png"
                          , height $ V 17
                          , width $ V 20
                          , margin $ MarginRight 7
                          ]
                          , textView
                          ([ text (getString HELP_AND_SUPPORT)
                           , color Color.black800
                           , weight 1.0
                           ] <> FontStyle.body1 TypoGraphy
                          )
                          , imageView
                          [ width $ V 18
                          , height $ V 18
                          , imageWithFallback $ "ny_ic_chevron_right_grey," <> (getAssetStoreLink FunctionCall) <> "ny_ic_chevron_right_grey.png"
                          ]
                        ]
                      ]
                    ]
                  ]
                , issueReportedView state push
              ]
            ]
        ]
      
   
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , margin (MarginTop 16)
      , gravity BOTTOM
      , visibility if state.props.reportIssue then VISIBLE else GONE
      , alignParentBottom "true,-1"
      ][PrimaryButton.view (push <<< PrimaryButtonActionController state ) (primaryButtonConfig state)]
    ]

---------------------- tripDetails ---------------------------

tripDetailsView ::  forall w . ST.TripDetailsScreenState -> PrestoDOM (Effect Unit) w
tripDetailsView state =
  linearLayout
  [ height $ V 40
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  ][  frameLayout
      [ height MATCH_PARENT
      , width WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER_VERTICAL
      ][ imageView
          [ imageWithFallback $ if (state.data.vehicleType == "AUTO_RICKSHAW" && getMerchant FunctionCall == YATRI) then "ny_ic_auto1," <> (getAssetStoreLink FunctionCall) <> "ny_ic_auto1.png"
                                else "ic_vehicle_front," <> (getAssetStoreLink FunctionCall) <> "ic_vehicle_front.png"
          , width (V 36)
          , height (V 36)
          ]
        ]
    , linearLayout 
      [ height MATCH_PARENT
      , width WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER_VERTICAL
      , margin (MarginLeft 10)
      ][ linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER_VERTICAL
          ][  textView $
              [ text state.data.date
              , color Color.black800
              ] <> FontStyle.body1 TypoGraphy
            , textView
              [ background Color.greyishBlue
              , cornerRadius 2.5
              , margin (Margin 5 0 5 0)
              , height (V 5)
              , width (V 5)
              ] 
            , textView $
              [ text state.data.time
              , color Color.black800
              ] <> FontStyle.body1 TypoGraphy
            ]
        ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity RIGHT
      , orientation VERTICAL
      ][  textView $
          [ text $ (getValueFromConfig "currency") <> ( show state.data.totalAmount)
          , color Color.black
          ] <> FontStyle.body14 TypoGraphy
        , textView $
          [ text $ getString(PAID)<> "  " <> if state.data.paymentMode == ST.CASH then (getString BY_CASH) else (getString ONLINE_)
          , color Color.greyDarker
          ] <> FontStyle.body16 TypoGraphy
        ]
    ]

------------------- separator -------------------
separatorView ::  forall w . PrestoDOM (Effect Unit) w
separatorView =
  linearLayout
  [ height (V 1)
  , width MATCH_PARENT
  , margin (Margin 0 16 0 16)
  , background Color.lightGreyShade
  ][]

tripDataView ::  forall w . (Action -> Effect Unit) ->  ST.TripDetailsScreenState -> PrestoDOM (Effect Unit) w
tripDataView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER_VERTICAL
  ][  linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        ][ linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation VERTICAL
            , weight 0.75
            ][ textView $
                [ text (getString TRIP_ID)
                , color Color.black700
                , margin (MarginBottom 4) 
                ] <> FontStyle.body5 TypoGraphy
              , linearLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , orientation HORIZONTAL
                , onClick push (const Copy)
                ][ textView $
                    [ text state.data.tripId
                    , width WRAP_CONTENT
                    , color Color.black900
                    ] <> FontStyle.body14 TypoGraphy
                  , imageView
                    [ imageWithFallback $ "ny_ic_copy," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_copy.png"
                    , height (V 15)
                    , width (V 13)
                    , margin (Margin 10 5 0 0)
                    
                    ]

                ]
              
            ]
          , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation VERTICAL
            , weight 0.25
            , visibility GONE
            ][ textView $
                [ text (getString RIDER)
                , color Color.black700
                , margin (MarginBottom 4) 
                ] <> FontStyle.body5 TypoGraphy
              , textView $
                [ text state.data.rider
                , color Color.black900
                ] <> FontStyle.body14 TypoGraphy
            ]
        ] 
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        , margin (MarginTop 20)
        , visibility if state.data.status == "CANCELLED" then GONE else VISIBLE
        ][ linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation VERTICAL
            , weight 0.865
            ][ textView $
                [ text (getString DISTANCE)
                , color Color.black700
                , margin (MarginBottom 4) 
                ] <> FontStyle.body5 TypoGraphy
              , textView $
                [ text (state.data.distance <> " km")
                , color Color.black900
                ] <> FontStyle.body14 TypoGraphy
            ]
          , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation VERTICAL
            , weight 0.135
            , visibility GONE
            ][ textView $
                [ text (getString TIME_TAKEN)
                , color Color.black700
                , margin (MarginBottom 4)
                ] <> FontStyle.body5 TypoGraphy
              , textView $
                [ text state.data.timeTaken
                , color Color.black900
                ] <> FontStyle.body14 TypoGraphy
            ]
        ] 
  ]


----------------- report Isssue ----------------
reportIssueView ::  forall w . ST.TripDetailsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
reportIssueView state push = 
  linearLayout
    [ orientation VERTICAL
    , width MATCH_PARENT
    , height WRAP_CONTENT
    , visibility GONE
    ][  linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        , orientation HORIZONTAL
        , margin (MarginBottom 16)
        , onClick push $ const ReportIssue 
        ][  textView
            ([ text (getString REPORT_AN_ISSUE)
            , color Color.darkDescriptionText
            ] <> FontStyle.body1 TypoGraphy)
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity RIGHT
            ][  imageView
                [ imageWithFallback if state.props.reportIssue then "ny_ic_chevron_up," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_chevron_up.png" else "ny_ic_chevron_down," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_chevron_down.png"
                , height $ V 5
                , width $ V 10 
                ]
              ] 
          ]
          , PrestoAnim.animationSet[
            Anim.fadeIn state.props.reportIssue
          , Anim.fadeOut $ not state.props.reportIssue
          ] $ scrollView
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              ][  linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , visibility if state.props.reportIssue then VISIBLE else GONE
                ][ linearLayout 
                   [ width MATCH_PARENT
                   , height $ V 120
                   , orientation HORIZONTAL
                   , cornerRadius 5.0
                   , padding ( Padding 2 2 2 2)
                   , gravity LEFT
                   , stroke "1,#E4E4E4"
                   , margin (MarginBottom 75)
                   ][  editText
                       [ height WRAP_CONTENT
                       , width WRAP_CONTENT
                       , weight 1.0
                       , textSize FontSize.a_14
                       , padding (Padding 14 14 14 14)
                       , color Color.black800
                       , background Color.white900
                       , fontStyle $ FontStyle.bold LanguageStyle
                       , text ""
                       , hint "You can describe the issue you faced here"
                       , pattern "[A-Za-z0-9 ]*,255"
                       , onChange push $ MessageTextChanged
                       ]
                     ]  
                ]
        ]
    ]   

-------------------------- issueReportedView -----------------------

issueReportedView ::  forall w . ST.TripDetailsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
issueReportedView state push = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  , visibility if state.props.issueReported then VISIBLE else GONE
  ][ linearLayout[
    height $ V (2 * (EHC.screenHeight unit )/3)
    , width MATCH_PARENT
    , gravity CENTER
    , orientation VERTICAL
  ][ imageView
      [ imageWithFallback $ "ny_ic_greetings," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_greetings.png"
      , height $ V 81
      , width $ V 135
      , margin (MarginBottom 32)
      ]
    , textView $
      [ text (getString THANK_YOU_FOR_WRITING_TO_US)
      , color Color.black900
      , margin (MarginBottom 12)
      ] <> FontStyle.h1 TypoGraphy
    , textView $ 
      [ text (getString WE_HAVE_RECIEVED_YOUR_ISSUE)
      , margin (Margin 42 0 42 0)
      , gravity CENTER
      , color Color.blackLightGrey
      ] <> FontStyle.body3 TypoGraphy]
  ]
