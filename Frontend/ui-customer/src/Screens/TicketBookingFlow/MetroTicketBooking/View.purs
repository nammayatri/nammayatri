module Screens.TicketBookingFlow.MetroTicketBooking.View where

import PrestoDOM
import Screens.Types as ST
import Styles.Colors as Color
import Effect (Effect)
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
-- import Components.SelectionTabModal.View as SelectionTabModal
import Components.PrimaryButton as PrimaryButton
-- import Components.IncrementDecrementModel as IncrementDecrementModel
import Components.PrimaryEditText.View as PrimaryEditText
-- import Components.IncrementDecrementModel.View as IncrementDecrementModel
-- import Components.GenericHeader.View as 
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Prelude
import Screens.TicketBookingFlow.MetroTicketBooking.Controller
import Screens.TicketBookingFlow.MetroTicketBooking.ComponentConfig
import Font.Style as FontStyle
import PrestoDOM.Animation as PrestoAnim
import Debug (spy)
import Engineering.Helpers.Commons as EHC
import Animation.Config
import JBridge as JB
import Data.Array
import Font.Size as FontSize
import Data.Maybe (maybe)

screen :: ST.MetroTicketBookingScreenState -> Screen Action ST.MetroTicketBookingScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "MetroTicketBookingScreen"
  , globalEvents : []
  , eval : \action state -> do
        let _ = spy "MetroTicketBookingScreenState action " action
        let _ = spy "MetroTicketBookingScreenState state " state
        eval action state
  }

view :: forall w . (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
view push state =
    PrestoAnim.animationSet [Anim.fadeIn true]  $ frameLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.black6000
    , onBackPressed push $ const BackPressed
    ]
    [ linearLayout 
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , background Color.grey700
        , orientation VERTICAL
        , visibility VISIBLE
        ]
        [ headerView state push
        , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin (Margin 16 24 16 20)
        , padding (Padding 20 20 20 20)
        , background Color.white900
        , cornerRadius 8.0
        , orientation VERTICAL
        ][ 
          linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , padding $ PaddingBottom 20 -- 20 20 20)
            ][  linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation HORIZONTAL
                -- , margin (Margin 16 12 16 0)
                , padding (Padding 2 2 2 2)
                , background Color.white900
                , stroke $ "1," <> Color.grey900
                , cornerRadius 30.0
                , gravity CENTER
                ][ selectionTab "One Way"  ST.ONE_WAY push state
                , selectionTab "Round Trip" ST.ROUND_TRIP push state
                ]
            ]
        -- , srcEditTextView push state
        , srcTextView push state
        -- , destEditTextView push state
        , destTextView push state
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity BOTTOM
            -- , onClick push $ const ToggleTermsAndConditions
                ][ textView $ 
                    [ text "Uncertain about metro routes?"
                    , color Color.black800
                    ] <> FontStyle.body1 TypoGraphy
                  , textView $ 
                    [ text " See Map"
                    , color Color.blue900
                    , onClick push $ const MetroRouteMapAction
                    -- , onClick (\action -> do
                    --         _<- push action
                    --         _ <- JB.openUrlInApp $ "getTermsAndConditionsUrl state.data.placeInfo"
                    --         pure unit
                    --         ) (const NoAction)
                    ] <> FontStyle.subHeading1 TypoGraphy
                ]
        ]
        , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 16 16 16 16
        , cornerRadius 8.0
        , background Color.white900
        , orientation VERTICAL
        ][ incrementDecrementView push state
        ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity BOTTOM
            , margin $ MarginHorizontal 16 16
            , onClick push $ const ToggleTermsAndConditions
                ][  imageView
                    [ height $ V 16
                    , width $ V 16 
                    , layoutGravity "center_vertical"
                    , margin $ MarginRight 8
                    , imageWithFallback $ fetchImage FF_COMMON_ASSET (if state.props.termsAndConditionsSelected then "ny_ic_checked" else "ny_ic_unchecked")
                    ]
                  , textView $ 
                    [ text "I agree to the"
                    , color Color.black800
                    ] <> FontStyle.body1 TypoGraphy
                  , textView $ 
                    [ text " Terms & Conditions"
                    , color Color.blue900
                    -- , onClick (\action -> do
                    --         _<- push action
                    --         _ <- JB.openUrlInApp $ "getTermsAndConditionsUrl state.data.placeInfo"
                    --         pure unit
                    --         ) (const NoAction)
                    ] <> FontStyle.body1 TypoGraphy
                ]
        , termsAndConditionsView (getTermsAndConditions "") true
        ]
        , updateButtonView state push
    ]

selectionTab :: forall w . String  -> ST.TicketType -> (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
selectionTab _text ticketType push state =
  textView
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text _text
  , background if ticketType == state.data.ticketType then Color.black900 else Color.white900 -- if leaderBoardType == state.props.leaderBoardType then Color.black900 else Color.grey800
  , padding (Padding 8 8 8 8)
  , weight 1.0
  , gravity CENTER
  , color if ticketType == state.data.ticketType then Color.white900 else Color.black900
  , cornerRadius 20.0
  , textSize FontSize.a_14
  , onClick (\action ->
              if state.data.ticketType /= ticketType then do
                _ <- push action
                pure unit
              else pure unit
            ) (const $ ChangeTicketTab ticketType)
  ]

termsAndConditionsView :: forall w . Array String -> Boolean -> PrestoDOM (Effect Unit) w
termsAndConditionsView termsAndConditions isMarginTop =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  -- , margin $ if isMarginTop then MarginTop 10 else MarginTop 0
  , margin $ Margin 16 13 16 0
  ] (mapWithIndex (\index item ->
      linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      ][ textView $
         [ textFromHtml $ " &#8226;&ensp; " <> item
         , color Color.black700
         ] <> FontStyle.tags TypoGraphy
      ]
  ) termsAndConditions )

getTermsAndConditions :: forall w . String -> Array String
getTermsAndConditions _ = ["Cancellation of tickets is not applicable" ,"The tickets can be purchased between 4:30 am to 22:30 pm on all days."]

headerView :: forall w. ST.MetroTicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , padding (PaddingTop EHC.safeMarginTop)
    ][  GenericHeader.view (push <<< GenericHeaderAC) (metroTicketBookingHeaderConfig state)
      , linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , gravity RIGHT
        , background Color.white900
        ][ linearLayout
          [ width WRAP_CONTENT
          , height MATCH_PARENT
          , gravity CENTER -- $ case state.data.config.profileEditGravity of 
          --     "bottom" -> BOTTOM
          --     _ -> CENTER
          , orientation VERTICAL
          , padding $ PaddingRight 16
          ][ textView $
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text "My Tickets"--(getString EDIT)
              , accessibilityHint $ "Edit Profile : Button"
              , accessibility ENABLE
              , color Color.blueTextColor
              , padding $ PaddingBottom 0 --(if state.data.config.profileEditGravity == "bottom" then 10 else 0)
              , onClick push (const $ MyMetroTicketAction)
              ] <> FontStyle.subHeading1 LanguageStyle
            ]
          ]
      ]

incrementDecrementView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
incrementDecrementView push state =
  let ticketLimit = 6
  in
  PrestoAnim.animationSet [
    Anim.translateInYAnim translateYAnimConfig { duration = 3000 , fromY = -5, toY = 0}
  ] $ 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 8.0
  , orientation VERTICAL
  , margin $ Margin 16 20 16 20
  ][  textView $
      [ text "No of Passengers"--"pcCategory.title "
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
          , padding $ Padding 28 1 28 7
          , onClick push $ const (DecrementTicket)-- pcCategory ticketLimit)
          , height WRAP_CONTENT
          ] <> FontStyle.body10 TypoGraphy
        , textView $
          [ background Color.white900
          , text $ show state.data.ticketCount--"$ show pcCategory.currentValue"
          , height WRAP_CONTENT
          , color Color.black800
          , weight 1.0
          , gravity CENTER
          ] <> FontStyle.body13 TypoGraphy
        , textView $
          [ background Color.black900
          , text "+"
          , color Color.yellow900
          , padding $ Padding 28 1 28 7
          , cornerRadius 4.0
          , onClick push $ const (IncrementTicket ) --pcCategory ticketLimit)
          , width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity CENTER
          ] <> FontStyle.body10 TypoGraphy
      ]
    , linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , orientation HORIZONTAL
              , gravity CENTER_VERTICAL
              -- , visibility if state.data.nameErrorMessage /= Nothing then VISIBLE else GONE
              ][  imageView $
                  [ width $ V 20
                  , height MATCH_PARENT
                  , padding $ Padding 0 5 0 3
                  , imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_grey" 
                  ]
                , textView $
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , text "Maximum 6 tickets are allowed per user."
                  , color Color.black600
                  -- , fontStyle $ FontStyle.bold LanguageStyle
                  , gravity LEFT
                  , margin $ Margin 0 0 0 0
                  , lineHeight "28"
                  , singleLine true
                  , alpha 1.0
                  ]  <> FontStyle.body3 TypoGraphy
              ]
    -- , updateButtonView state push
  ]

srcEditTextView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
srcEditTextView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginBottom 16
  -- , accessibility if state.props.genderOptionExpanded then DISABLE_DESCENDANT else DISABLE
  -- , accessibilityHint "Name edit Text field"
  ][PrimaryEditText.view (push <<< SourceEditText) (metroSrcEditText state)]

destEditTextView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
destEditTextView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginBottom 16
  -- , accessibility if state.props.genderOptionExpanded then DISABLE_DESCENDANT else DISABLE
  -- , accessibilityHint "Name edit Text field"
  ][PrimaryEditText.view (push <<< DestinationEditText) (metroDestEditText state)]

srcTextView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
srcTextView push state =
  linearLayout 
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginBottom 16
    ][ textView $ 
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text "From"
          , color Color.black900
          , gravity LEFT
          , lineHeight "28"
          , singleLine true
          , margin $ Margin 0 0 0 10
          , alpha 1.0
          , accessibility ENABLE
          ] <> (FontStyle.getFontStyle FontStyle.Body3 LanguageStyle)
    , linearLayout
    [ height $ V 54
    , width MATCH_PARENT
    , background Color.white900
    , cornerRadius 5.0
    , gravity CENTER_VERTICAL
    , onClick push $ const SelectSource
    , stroke ("1," <> Color.borderColorLight)
    ][
      textView $ 
        [ height MATCH_PARENT
        , width WRAP_CONTENT
        , text state.data.srcLoc  -- $ maybe "Starting From?" (_.stationName) state.data.srcLoc 
        , color Color.black800
        , gravity CENTER_VERTICAL
        , lineHeight "28"
        , singleLine true
        , margin $ Margin 20 0 10 0
        , alpha 1.0
        ] <> (FontStyle.getFontStyle FontStyle.SubHeading1 LanguageStyle)
    ]
    ]

destTextView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
destTextView push state =
  linearLayout 
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginBottom 16
    ][ textView $ 
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text "To"
          , color Color.black900
          , gravity LEFT
          , lineHeight "28"
          , singleLine true
          , margin $ Margin 0 0 0 10
          , alpha 1.0
          , accessibility ENABLE
          ] <> (FontStyle.getFontStyle FontStyle.Body3 LanguageStyle)
    , linearLayout
    [ height $ V 54
    , width MATCH_PARENT
    , background Color.white900
    , cornerRadius 5.0
    , gravity CENTER_VERTICAL
    , onClick push $ const SelectDestination
    , stroke ("1," <> Color.borderColorLight)
    ][
      textView $ 
        [ height MATCH_PARENT
        , width WRAP_CONTENT
        , text state.data.destLoc  -- $ maybe "Where to?" (_.stationName) state.data.destLoc 
        , color Color.black800
        , gravity CENTER_VERTICAL
        , lineHeight "28"
        , singleLine true
        , margin $ Margin 20 0 10 0
        , alpha 0.5
        ] <> (FontStyle.getFontStyle FontStyle.SubHeading1 LanguageStyle)
    ]
    ]

updateButtonView :: forall w. ST.MetroTicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
updateButtonView state push = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity BOTTOM
  , alignParentBottom "true,-1"
  , background Color.transparent
  ][ linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , width MATCH_PARENT
      , background Color.white900
      , padding $ PaddingVertical 5 24
      ][ PrimaryButton.view (push <<< UpdateButtonAction) (updateButtonConfig state)]
    ]