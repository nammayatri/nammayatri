{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTIEHULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HelpAndSupportScreen.View where

import Common.Types.App
import Screens.CustomerUtils.HelpAndSupportScreen.ComponentConfig

import Animation as Anim
import Engineering.Helpers.Utils as EHU
import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.SourceToDestination as SourceToDestination
import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array as DA
import Data.Either (Either(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, discard, map, pure, unit, show, not, ($), (-), (/=), (<<<), (<=), (<>), (==), (||), (<), (<>))
import Presto.Core.Types.Language.Flow (Flow, doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..), afterRender, alignParentRight, background, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, stroke, text, textSize, textView, visibility, width, imageWithFallback, weight, layoutGravity, clickable, alignParentBottom, scrollView, adjustViewWithKeyboard, lineHeight, singleLine, alpha, accessibility, accessibilityHint)
import PrestoDOM.Properties as PP
import PrestoDOM.Types.DomAttributes as PTD
import Screens.HelpAndSupportScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Services.API (RideBookingListRes(..))
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (GlobalState, defaultGlobalState)
import Common.Types.App
import Screens.CustomerUtils.HelpAndSupportScreen.ComponentConfig
import Components.PrimaryEditText as PrimaryEditText
import Components.PrimaryButton as PrimaryButton
import Data.String as DS
import Engineering.Helpers.MobilityPrelude

screen :: ST.HelpAndSupportScreenState -> Screen Action ST.HelpAndSupportScreenState ScreenOutput
screen initialState =
  {
    initialState
  , view
  , name : "HelpAndSupportScreen"
  , globalEvents : [
      (\push -> do
              if (isStrEmpty initialState.data.source) then
                launchAff_ $ void $ EHC.flowRunner defaultGlobalState $ getPastRides RideBookingListAPIResponseAction push initialState
              else
                pure unit
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
 ]$[ linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , accessibility if state.props.showDeleteAccountView || state.props.isCallConfirmation || DA.any (_ == state.data.accountStatus) [ ST.CONFIRM_REQ , ST.DEL_REQUESTED ] then DISABLE_DESCENDANT else DISABLE
    , padding $ Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom
    , onBackPressed push $ const BackPressed state.props.isCallConfirmation
    , afterRender push (const AfterRender)
    ][  GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , visibility if state.props.apiFailure || state.data.isNull then GONE else VISIBLE
        , background Color.catskillWhite
        ][  textView $
            [ text (getString YOUR_RECENT_RIDE)
            , color Color.darkCharcoal
            , width WRAP_CONTENT
            , margin (Margin 16 12 0 12)
            ] <> FontStyle.subHeading2 LanguageStyle
          , textView $
            [ text (getString VIEW_ALL_RIDES)
            , alignParentRight "true,-1"
            , margin (Margin 0 14 16 14)
            , accessibilityHint "View All Rides : Button"
            , accessibility ENABLE
            , width MATCH_PARENT
            , gravity RIGHT
            , color Color.blue900
            , onClick push $ const ViewRides
            ] <> FontStyle.body1 LanguageStyle
          ]
      , recentRideView state push
      , headingView state (getString ALL_TOPICS)
      , allTopicsView state push
      , apiFailureView state push
      ]
     , deleteAccountView state push
  ] <> (if state.props.isCallConfirmation then [PopUpModal.view (push <<< PopupModelActionController) (callConfirmationPopup state)] else [])
    <> (if state.data.accountStatus == ST.CONFIRM_REQ then [PopUpModal.view (push <<<  PopUpModalAction) (requestDeletePopUp state )] else [])
    <> (if state.data.accountStatus == ST.DEL_REQUESTED then [PopUpModal.view (push <<< AccountDeletedModalAction) (accountDeletedPopUp state)] else [])

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
  , height WRAP_CONTENT
  , visibility if state.data.isNull || state.props.apiFailure then GONE else VISIBLE
  ][
    linearLayout
    [ height $ if EHC.os == "IOS" then V 134 else WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    ][  imageView
        [ background Color.greyLight
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_help_map"
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
    ][
    textView $
        [ text (getString REPORT_AN_ISSUE_WITH_THIS_TRIP)
        , accessibilityHint "Report An Issue With This Trip : Button"
        , accessibility ENABLE 
        , color Color.blue900
        ] <> FontStyle.tags LanguageStyle
     ,  linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity RIGHT
        ][  imageView
            [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right"
            , height $ V 15
            , width $ V 15
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
  ][  textView $
      [ text state.data.date
      , color Color.greyShade
      ] <> FontStyle.body16 LanguageStyle
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
    , textView $
      [ text state.data.time
      , color Color.greyShade
      ] <> FontStyle.body16 LanguageStyle
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
  ][  textView $
      [ text (getString YOU_RATED)
      , color Color.darkCharcoal
      , accessibilityHint $ "You Rated : " <> (show state.data.rating) <> " stars"
      , accessibility ENABLE
      ] <> FontStyle.body3 LanguageStyle
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
                            , imageWithFallback $ fetchImage FF_COMMON_ASSET $ if item <= state.data.rating then "ny_ic_star_active" else "ny_ic_star_inactive"
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
              , textView $
                [ accessibilityHint $ item.title <> " : Button"
                , accessibility ENABLE
                , text item.title
                , color Color.darkCharcoal
                , margin (MarginLeft 13)
                ] <> FontStyle.paragraphText LanguageStyle
              , linearLayout
                [ height WRAP_CONTENT
                , weight 1.0
                , gravity RIGHT
                , layoutGravity "center_vertical"
                ][  imageView
                    [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right"
                    , height $ V 15
                    , width $ V 15  
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

deleteAccountView :: ST.HelpAndSupportScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
deleteAccountView state push=
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , visibility if state.props.showDeleteAccountView then VISIBLE else GONE
  , padding $ PaddingTop EHC.safeMarginTop
  , background Color.white900
  , accessibility if DA.any (_ == state.data.accountStatus) [ ST.CONFIRM_REQ , ST.DEL_REQUESTED ] then DISABLE_DESCENDANT else DISABLE
  , clickable true
  ][
    GenericHeader.view (push <<< DeleteGenericHeaderAC) (deleteGenericHeaderConfig state) 
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding (Padding 16 12 16 12)
    , background Color.blue600
    ][ 
      textView
      [ text if state.props.btnActive || state.data.accountStatus == ST.DEL_REQUESTED then (getString WE_WOULD_APPRECIATE_YOUR_REASONING) else (getString WE_WOULD_APPRECIATE_YOUR_FEEDBACK)
      , textSize FontSize.a_12
      , fontStyle $ FontStyle.regular LanguageStyle
      , color Color.black650
      ]
    ]
  , relativeLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , adjustViewWithKeyboard "true"
    ][  editTextView state push
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background Color.white900
        , alignParentBottom "true,-1"
        , weight 1.0 
        ][PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfigSubmitRequest state)]
      ]
    ]

editTextView :: ST.HelpAndSupportScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
editTextView state push =
  linearLayout[
    height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][
    scrollView[
      width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , padding (Padding 6 0 6 60)
      , orientation VERTICAL
      ][
          PrimaryEditText.view (push <<< EmailEditTextAC) (primaryEditTextConfigEmail state)
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          ][  PrimaryEditText.view (push <<< DescriptionEditTextAC) (primaryEditTextConfigDescription state)
            , linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation HORIZONTAL
              , gravity CENTER_VERTICAL
              , padding $ PaddingLeft 10
              , margin $ MarginTop 5
              , visibility if ((HU.strLenWithSpecificCharacters state.data.description  "[a-zA-Z]") < 10) then VISIBLE else GONE
              ][  imageView $
                  [ width $ V 24
                  , height $ V 24
                  , padding $ PaddingVertical 5 3
                  , margin $ MarginRight 3
                  , imageWithFallback $ fetchImage FF_ASSET "ny_ic_info" -- https://assets.juspay.in/nammayatri/images/user/ny_ic_information_grey.png" 
                  ]
                , textView $
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , text $ getString DESCRIPTION_SHOULD_BE_MORE_THAN_10_ALPHABETIC_CHARACTERS
                  , color Color.black600
                  , gravity LEFT
                  , margin $ Margin 0 0 0 0
                  , lineHeight "28"
                  , singleLine true
                  , alpha 1.0
                  ]  <> FontStyle.body3 TypoGraphy
              ] ]
      ]
    ]
  ]

headingView :: ST.HelpAndSupportScreenState -> String -> forall w . PrestoDOM (Effect Unit) w
headingView state title =
  textView $
    [ text title
    , width MATCH_PARENT
    , visibility if state.props.apiFailure then GONE else VISIBLE
    , height WRAP_CONTENT
    , padding (Padding 16 12 0 12)
    , background Color.catskillWhite
    , color Color.darkCharcoal
    ] <> FontStyle.body5 LanguageStyle

getPastRides :: forall action.( RideBookingListRes -> String -> action) -> (action -> Effect Unit) -> ST.HelpAndSupportScreenState ->  Flow GlobalState Unit
getPastRides action push state = do
  void $ EHU.loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
  void $ EHU.toggleLoader true
  (rideBookingListResponse) <- Remote.rideBookingList "8" "0" "false"
  void $ EHU.toggleLoader false
  case rideBookingListResponse of
      Right (RideBookingListRes  listResp) -> doAff do liftEffect $ push $ action (RideBookingListRes listResp) "success"
      Left (err) -> doAff do liftEffect $ push $ action (RideBookingListRes dummyListResp ) "failure"

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
    , image : fetchImage FF_COMMON_ASSET "ny_ic_clip_board"
    }
  ] <> if state.data.config.enableContactSupport then 
        [ { action : CallSupport
          , title : (getString CONTACT_SUPPORT)
          , image : fetchImage FF_ASSET "ny_ic_help"
          } ] else []
    <> if state.data.config.showDeleteAccount then 
        [ { action : DeleteAccount
          , title : (getString REQUEST_TO_DELETE_ACCOUNT)
          , image : "ny_ic_delete_account,https://assets.juspay.in/beckn/merchantcommon/images/ny_ic_delete_account.png" --update here
          } ] else []