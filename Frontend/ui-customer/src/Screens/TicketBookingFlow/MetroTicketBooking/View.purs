module Screens.TicketBookingFlow.MetroTicketBooking.View where

import PrestoDOM
import Screens.Types as ST
import Styles.Colors as Color
import Effect (Effect)
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText.View as PrimaryEditText
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
import Effect.Aff (launchAff)
import Services.API
import Presto.Core.Types.Language.Flow (doAff, Flow, delay)
import Types.App (GlobalState, defaultGlobalState)
import Screens.Types
import Services.Backend as Remote
import Data.Either (Either(..))
import Effect.Class (liftEffect)
import Data.Time.Duration (Milliseconds(..))

screen :: ST.MetroTicketBookingScreenState -> Screen Action ST.MetroTicketBookingScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "MetroTicketBookingScreen"
  , globalEvents : [getQuotes]
  , eval : \action state -> do
        let _ = spy "MetroTicketBookingScreenState action " action
        let _ = spy "MetroTicketBookingScreenState state " state
        eval action state
  }
  where
    getQuotes push = do
      void $ launchAff $ EHC.flowRunner defaultGlobalState $ getQuotesPolling initialState.data.searchId 5 3000.0 initialState push GetMetroQuotesAction
      pure $ pure unit

getQuotesPolling :: forall action. String-> Int -> Number -> ST.MetroTicketBookingScreenState -> (action -> Effect Unit) -> (Array MetroQuote -> action) -> Flow GlobalState Unit
getQuotesPolling searchId count delayDuration state push action = do
  if state.props.currentStage == GetMetroQuote && searchId /= "" then do
    if count > 0 then do
      (getMetroQuotesResp) <- Remote.getMetroQuotes searchId
      case getMetroQuotesResp of
          Right (GetMetroQuotesRes resp) -> do
            if (not (null resp)) then do
                doAff do liftEffect $ push $ action resp
            else do
              if (count == 1) then do
                doAff do liftEffect $ push $ action resp
              else do
                void $ delay $ Milliseconds delayDuration
                getQuotesPolling searchId (count - 1) delayDuration state push action
          Left _ -> pure unit
      else 
        pure unit
  else pure unit

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
        ]
        [ headerView state push
        , linearLayout
          [ height $ V 1
          , width MATCH_PARENT
          , background Color.greySmoke
          ][]
        , infoSelectioView state push
        ]
        , updateButtonView state push
    ]

infoSelectioView :: forall w . ST.MetroTicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
infoSelectioView state push =
    scrollView
      [ height if EHC.os == "IOS" then V (EHC.screenHeight unit) else MATCH_PARENT
      , width MATCH_PARENT    
      , padding $ PaddingBottom 75
      , scrollBarY false
      ] [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          ]
          [
            linearLayout
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
                        , padding $ PaddingBottom 20
                        ][  linearLayout
                            [ width MATCH_PARENT
                            , height WRAP_CONTENT
                            , orientation HORIZONTAL
                            , padding (Padding 2 2 2 2)
                            , background Color.white900
                            , stroke $ "1," <> Color.grey900
                            , cornerRadius 30.0
                            , gravity CENTER
                            ][ selectionTab "One Way"  ST.ONE_WAY push state
                            , selectionTab "Round Trip" ST.ROUND_TRIP push state
                            ]
                        ]
                    , srcTextView push state
                    , destTextView push state
                    , linearLayout
                        [ height WRAP_CONTENT
                        , width MATCH_PARENT
                        , gravity BOTTOM
                            ][ textView $ 
                                [ text "Uncertain about metro routes?"
                                , color Color.black800
                                ] <> FontStyle.body1 TypoGraphy
                              , textView $ 
                                [ text " See Map"
                                , color Color.blue900
                                , onClick push $ const MetroRouteMapAction
                                ] <> FontStyle.subHeading1 TypoGraphy
                            ]
                    ]
                  , incrementDecrementView push state
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
                              --         _ <- JB.openUrlInApp $ "url" -- uncomment once doc link is available
                              --         pure unit
                              --         ) (const NoAction)
                              ] <> FontStyle.body1 TypoGraphy
                          ]
                  , termsAndConditionsView (getTermsAndConditions "") true
          ]
      ]

selectionTab :: forall w . String  -> ST.TicketType -> (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
selectionTab _text ticketType push state =
  textView
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text _text
  , background if ticketType == state.data.ticketType then Color.black900 else Color.white900
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
          , gravity CENTER
          , orientation VERTICAL
          , padding $ PaddingRight 16
          ][ textView $
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text "My Tickets"--(getString MY_TICKETS)
              , accessibilityHint $ "My Tickets : Button"
              , accessibility ENABLE
              , color Color.blueTextColor
              , onClick push (const $ MyMetroTicketAction)
              ] <> FontStyle.subHeading1 LanguageStyle
            ]
          ]
      ]

incrementDecrementView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
incrementDecrementView push state =
  let ticketLimit = 6
  in 
  linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 16 0 16 16
        , cornerRadius 8.0
        , background Color.white900
        , orientation VERTICAL
        ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , cornerRadius 8.0
      , orientation VERTICAL
      , margin $ Margin 16 20 16 20
      ][  textView $
          [ text "No of Passengers"
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
              , onClick push $ const (DecrementTicket)
              , height WRAP_CONTENT
              ] <> FontStyle.body10 TypoGraphy
            , textView $
              [ background Color.white900
              , text $ show state.data.ticketCount
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
              , onClick push $ const (IncrementTicket )
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
                  ][  imageView $
                      [ width $ V 20
                      , height MATCH_PARENT
                      , padding $ PaddingVertical 5 3
                      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_grey" 
                      ]
                    , textView $
                      [ height WRAP_CONTENT
                      , width WRAP_CONTENT
                      , text "Maximum 6 tickets are allowed per user."
                      , color Color.black600
                      , gravity LEFT
                      , singleLine true
                      , alpha 1.0
                      ]  <> FontStyle.body3 TypoGraphy
                  ]
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

textViewForLocation :: forall w. String -> ST.LocationActionId -> (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
textViewForLocation label actionId push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginBottom 16
    ][ textView $ 
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text label
          , color Color.black900
          , gravity LEFT
          , singleLine true
          , margin $ MarginBottom 10
          , alpha 1.0
          , accessibility ENABLE
          ] <> (FontStyle.getFontStyle FontStyle.Body3 LanguageStyle)
    , linearLayout
    [ height $ V 54
    , width MATCH_PARENT
    , background Color.white900
    , cornerRadius 5.0
    , gravity CENTER_VERTICAL
    , onClick push $ const (SelectLocation actionId)
    , stroke ("1," <> Color.borderColorLight)
    ][
      textView $ 
        [ height MATCH_PARENT
        , width WRAP_CONTENT
        , text $ if actionId == Src then 
                    if state.data.srcLoc == "" then "Starting From?" else state.data.srcLoc
                  else 
                    if state.data.destLoc == "" then "Where to?" else state.data.destLoc
        , color Color.black800
        , gravity CENTER_VERTICAL
        , singleLine true
        , margin $ MarginHorizontal 20 10
        , alpha $ if actionId == Src then 
                    if state.data.srcLoc == "" then 0.5 else 1.0
                  else 
                    if state.data.destLoc == "" then 0.5 else 1.0
        ] <> (FontStyle.getFontStyle FontStyle.SubHeading1 LanguageStyle)
    ]
    ]

srcTextView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
srcTextView = textViewForLocation "From" Src

destTextView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
destTextView = textViewForLocation "To" Dest
