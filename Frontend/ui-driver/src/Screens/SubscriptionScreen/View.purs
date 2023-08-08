module Screens.SubscriptionScreen.View where

import Screens.SubscriptionScreen.ComponentConfig

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.BottomNavBar (navData)
import Components.BottomNavBar as BottomNavBar
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe as Mb
import Data.String as DS
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (flowRunner, screenWidth)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getAssetStoreLink, getImageUrl)
import Helpers.Utils as HU
import Prelude (Unit, const, map, not, show, unit, ($), (&&), (*), (-), (/), (/=), (<<<), (<>), (==), (>), bind, pure, discard, void)
import Presto.Core.Types.Language.Flow (Flow, doAff, getState)
import PrestoDOM (Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), afterRender, alignParentBottom, background, color, cornerRadius, ellipsize, fontStyle, gradient, gravity, height, imageView, imageWithFallback, lineHeight, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, scrollBarY, scrollView, stroke, text, textFromHtml, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens as ScreenNames
import Screens.SubscriptionScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.SubscriptionScreen.ScreenData (dummyPlanConfig)
import Screens.Types (AutoPayStatus(..), MyPlanData, PaymentMethod(..), PlanCardConfig, PromoConfig, SubscriptionScreenState, SubscriptionSubview(..), GlobalProps)
import Services.API (GetCurrentPlanResp(..), GetDriverInfoResp(..), UiPlansResp(..))
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (GlobalState(..), defaultGlobalState)


screen :: SubscriptionScreenState -> Screen Action SubscriptionScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "SubscriptionScreen"
  , globalEvents: [(\push -> do
      void $ launchAff $ flowRunner defaultGlobalState $ loadData push LoadPlans LoadMyPlans ShowError initialState
      pure (pure unit)
    )]
  , eval:
      ( \state action -> do
          let _ = spy "SubscriptionScreen ----- state" state
          let _ = spy "SubscriptionScreen --------action" action
          eval state action
      )
  }

loadData :: forall action. (action -> Effect Unit) ->  (UiPlansResp -> action) -> (GetCurrentPlanResp -> action) -> action -> SubscriptionScreenState -> Flow GlobalState Unit
loadData push loadPlans loadMyPlans errorAction state = do
  (GlobalState globalState) <- getState
  let globalProp = globalState.globalProps
  let (GetDriverInfoResp driverInfo) = globalProp.driverInformation
  if not driverInfo.subscribed then do
    uiPlans <- Remote.getUiPlans ""
    case uiPlans of
      Right resp -> doAff do liftEffect $ push $ loadPlans resp
      Left err -> doAff do liftEffect $ push $ errorAction
  else do
    currentPlan <- Remote.getCurrentPlan driverInfo.id
    case currentPlan of
      Right resp -> doAff do liftEffect $ push $ loadMyPlans resp
      Left err -> doAff do liftEffect $ push $ errorAction
  pure unit

view :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  , onBackPressed push $ const BackPressed
  , afterRender push $ const AfterRender
  , background Color.white900
  ][ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , gravity CENTER
      ][ Anim.screenAnimationFadeInOut $
          linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , weight 1.0
          ][ joinPlanView push state (state.props.subView == JoinPlan)
            , managePlanView push state (state.props.subView == ManagePlan)
            , myPlanView push state (state.props.subView == MyPlan)
            , autoPayDetailsView push state (state.props.subView == PlanDetails)
          ]
        , BottomNavBar.view (push <<< BottomNavBarAction) (navData ScreenNames.SUBSCRIPTION_SCREEN)
      ]
    , PrestoAnim.animationSet [ Anim.fadeIn (not Mb.isNothing state.props.popUpState) ] $
      linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , visibility if (not Mb.isNothing state.props.popUpState) then VISIBLE else GONE
      ][PopUpModal.view (push <<< PopUpModalAC) (pupupModalConfig state)]
  ]


joinPlanView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> Boolean -> PrestoDOM (Effect Unit) w
joinPlanView push state visibility' = 
  PrestoAnim.animationSet [ Anim.fadeIn visibility' ] $
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , visibility if visibility' then VISIBLE else GONE
  ][ headerView push "Join Namma Yatri" "" true
    , relativeLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , background Color.blue600
      ][ imageView
          [ width $ V 116
          , height $ V 368
          , imageWithFallback "ny_ic_ny_driver,"
          ]
        , enjoyBenefitsView push state
        , plansBottomView push state
      ]

  ]

enjoyBenefitsView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
enjoyBenefitsView push state = 
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , gravity RIGHT
    , orientation VERTICAL
    , margin $ Margin 0 30 30 0
    ][  linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , margin $ MarginTop 15
        , orientation VERTICAL
        ][ commonTV push "Enjoy these benefits " Color.black800 (FontStyle.subHeading2 TypoGraphy) 0 LEFT
          , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            ](map
                (\(item) ->
                    linearLayout
                      [ width MATCH_PARENT
                      , height WRAP_CONTENT
                      , gravity CENTER_VERTICAL
                      ][ imageView
                          [ imageWithFallback $ "ny_ic_check_green," <> (HU.getCommonAssetStoreLink FunctionCall) <> "ny_ic_check_green.png"
                          , width $ V 11
                          , height $ V 8
                          ]
                        , textView $
                          [ margin $ MarginLeft 11
                          , text item
                          , color Color.black700
                          , height WRAP_CONTENT
                          , width WRAP_CONTENT
                          ] <> FontStyle.body1 TypoGraphy
                      ]
                )
              ["ZERO commission", "Earn Today, Pay Tomorrow", "Pay only if you take rides"]
            )
        ]
    ]

plansBottomView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
plansBottomView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , alignParentBottom "true,-1"
  , cornerRadii $ Corners 20.0 true true false false
  , background Color.white900
  , padding $ Padding 20 20 20 0
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][ linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          ][ commonTV push "Choose your Plan!✨" Color.black800 (FontStyle.body8 TypoGraphy) 0 RIGHT
          , linearLayout
            [ weight 1.0
            , height WRAP_CONTENT
            , gravity RIGHT
            ][ imageView
                [ width $ V 91
                , height $ V 22
                , imageWithFallback "ny_ic_upi_autopay,"
                ]
            ]
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          ][ commonTV push "No charges till Aug31" Color.black800 (FontStyle.body8 TypoGraphy) 0 LEFT
            , textView $
              [ weight 1.0
              , height WRAP_CONTENT
              , gravity RIGHT
              , text "View Payment History"
              , color Color.blue900
              ] <> FontStyle.body1 TypoGraphy
          ]
        , scrollView
          [ width MATCH_PARENT
          , weight 1.0
          , margin $ MarginTop 10
          ][ linearLayout
              [ weight 1.0
              , width MATCH_PARENT
              , orientation VERTICAL
              ](map 
                  (\item -> planCardView push item (item.id == state.props.joinPlanProps.selectedPlan) ChoosePlan
                  ) state.data.joinPlanData.allPlans)
          ]
        , PrimaryButton.view (push <<< JoinPlanAC) (joinPlanButtonConfig state)
      ]
  ]

commonTV :: forall w. (Action -> Effect Unit) -> String -> String -> (forall properties. (Array (Prop properties))) -> Int -> Gravity -> PrestoDOM (Effect Unit) w
commonTV push text' color' fontStyle marginTop gravity' =
  textView $
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text text'
  , color color'
  , gravity gravity'
  , margin $ MarginTop marginTop
  ] <> fontStyle


managePlanView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> Boolean -> PrestoDOM (Effect Unit) w
managePlanView push state visibility' =
  PrestoAnim.animationSet [ Anim.fadeIn visibility' ] $
  relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , visibility if visibility' then VISIBLE else GONE
  ][ headerView push "Manage Plan" "" true
   , managePlanBodyView push state
   , linearLayout
     [ height $ V 45
     , width MATCH_PARENT
     , gravity CENTER
     , alignParentBottom "true,-1"
     , background Color.grey700
     , stroke $ "1," <> Color.grey900
     ][ textView
        [ textFromHtml "<u>View Autopay Details</u>"
        , textSize FontSize.a_12
        , padding $ Padding 5 5 5 5
        , fontStyle $ FontStyle.regular LanguageStyle
        , color Color.black800
        , onClick push $ const ViewAutopayDetails
        ]
      ]
   ]


myPlanView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> Boolean -> PrestoDOM (Effect Unit) w
myPlanView push state visibility' =
  PrestoAnim.animationSet [ Anim.fadeIn visibility' ] $
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , visibility if visibility' then VISIBLE else GONE
  , gradient (Linear 180.0 ["#E2EAFF", "#F5F8FF"])
  ][ headerView push "Plan" "<u>View Payment History</u>" false
   , myPlanBodyview push state
  ]

headerView :: forall w. (Action -> Effect Unit) -> String -> String -> Boolean -> PrestoDOM (Effect Unit) w 
headerView push title actionText backbutton =
  linearLayout
  [ height $ V 55
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  , padding $ PaddingHorizontal 16 16
  , background Color.white900
  , stroke $ "1," <> Color.grey900
  ][ imageView
     [ width $ V 24
     , height $ V 24
     , margin $ MarginRight 16
     , visibility if backbutton then VISIBLE else GONE
     , onClick push $ const $ BackPressed
     , imageWithFallback "ny_ic_chevron_left,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_left.png"
     ]
   , textView
     [ text title
     , textSize FontSize.a_18
     , fontStyle $ FontStyle.semiBold LanguageStyle
     , color Color.darkDescriptionText
     , padding $ PaddingBottom 4
     , weight 1.0
     ]
   , textView
     [ textFromHtml actionText
     , textSize FontSize.a_12
     , visibility if (DS.length actionText > 0) then VISIBLE else GONE
     , fontStyle $ FontStyle.medium LanguageStyle
     , onClick push $ const HeaderRightClick
     , color Color.blue900
     ]
  ]

myPlanBodyview :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w 
myPlanBodyview push state =
  scrollView  
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , scrollBarY false
  ][ linearLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , orientation VERTICAL
     , padding $ PaddingVertical 24 12
     ][ linearLayout
       [ height WRAP_CONTENT
       , width MATCH_PARENT
       , orientation HORIZONTAL
       , gravity CENTER_VERTICAL
       , margin $ Margin 16 0 16 16 
       ][ textView 
           [ text "My Plan"
           , textSize FontSize.a_20
           , fontStyle $ FontStyle.bold LanguageStyle
           , color Color.black800
           , weight 1.0
           ]
         , paymentMethodView push state.data.myPlanData
       ]
     , planDescriptionView push state
     , if true then alertView push (getImageURL "ny_ic_warning_red") Color.red "Low Account Balance" "Your bank account balance is low. Add at least ₹25 by 17 Aug 2023 to enjoy uninterrupted rides" "" NoAction else dummyView
     , if true then alertView push (getImageURL "ny_ic_warning_blue") Color.blue800 "Switch and Save" "You have completed over 7 rides today. Save up to ₹10 by switching to the DAILY UNLIMITED plan" "Switch Now" NoAction else dummyView
     , if true then alertView push (getImageURL "ny_ic_warning_blue") Color.blue800 "Payment mode changed to manual" "You have cancelled your UPI Autopay. You can clear your dues manually." "" NoAction else dummyView
     , duesView push state
     , if state.props.resumeBtnVisibility then PrimaryButton.view (push <<< ResumeAutoPay) (resumeAutopayButtonConfig state) else dummyView
    ]
  ]


planDescriptionView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w 
planDescriptionView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , cornerRadius 8.0
  , background Color.white900
  , stroke $ "1," <> Color.grey900
  , padding $ Padding 16 12 16 20
  , margin $ MarginHorizontal 16 16
  ][ linearLayout
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , gravity CENTER_VERTICAL
     , orientation HORIZONTAL
     , margin $ MarginBottom 5
     ][ textView
        [ text "DAILY UNLIMITED"
        , textSize FontSize.a_14
        , fontStyle $ FontStyle.extraBold LanguageStyle
        , color Color.black700
        , weight 1.0
        ]
      , textView
        [ text "₹25/ Day"
        , textSize FontSize.a_16
        , fontStyle $ FontStyle.bold LanguageStyle
        , color Color.black800
        ]
     ]
   , textView
     [ text "Enjoy UNLIMITED rides, every day!"
     , textSize FontSize.a_12
     , fontStyle $ FontStyle.medium LanguageStyle
     , color Color.black600
     , margin $ MarginBottom 12
     ]
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , margin $ MarginBottom 12
    ](map 
         (\item -> promoCodeView push item
         ) state.data.myPlanData.offers)
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ Padding 8 8 8 8
    , background Color.grey700
    , margin $ MarginBottom 16
    , cornerRadius 4.0
    ][ textView
       [ textFromHtml "Freedom offer : <b> ₹6/Day from Sep 1-30 </b> <br>Valid only if you join by <b> Aug 16. </b> <br> <b> No charges till Aug 31 </b>"
       , textSize FontSize.a_10
       , fontStyle $ FontStyle.semiBold LanguageStyle
       , color Color.black600
       , height WRAP_CONTENT
       , lineHeight "15"
       ]
    ]
  , arrowButtonView push "Manage Plan" true GotoManagePlan
  ]

duesView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w 
duesView push state = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , padding $ Padding 16 16 16 16
  , orientation VERTICAL
  , background Color.white900
  , cornerRadius 8.0
  , stroke $ "1," <> Color.grey900
  , margin $ Margin 16 16 16 0
  ][ textView
     [ text "Your Dues"
     , textSize FontSize.a_14
     , fontStyle $ FontStyle.semiBold LanguageStyle
     , color Color.black800
     , margin $ MarginBottom 8
     ]
   , textView
     [ text "You have set up an autopay to clear your dues.\nWe will automatically try to ensure that your dues are always paid on time."
     , textSize FontSize.a_12
     , fontStyle $ FontStyle.medium LanguageStyle
     , color Color.black600
     , margin $ MarginBottom 16
     ]
   , linearLayout
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , padding $ Padding 16 12 16 12
     , margin $ MarginBottom 12
     , orientation VERTICAL
     , background Color.blue600
     , cornerRadius 8.0
     ][ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        ][ textView
           [ text "Current Dues"
           , textSize FontSize.a_12
           , fontStyle $ FontStyle.medium LanguageStyle
           , color Color.black600
           , weight 1.0
           ] 
         , textView
           [ text "Your Limit"
           , textSize FontSize.a_12
           , fontStyle $ FontStyle.medium LanguageStyle
           , color Color.black600
           ]              
        ]
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        ][ textView
           [ text "₹71"
           , textSize FontSize.a_18
           , fontStyle $ FontStyle.bold LanguageStyle
           , color Color.blue800
           , weight 1.0
           ] 
         , textView
           [ text "₹100"
           , textSize FontSize.a_18
           , fontStyle $ FontStyle.bold LanguageStyle
           , color Color.black700
           ]             
        ]
      , linearLayout
        [ height $ V 4
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , margin $ MarginTop 8
        ][ linearLayout
           [ height $ V 4
           , width $ V (HU.clampNumber 71.0 100.0 ((screenWidth unit) - 100))
           , background Color.blue800
           , cornerRadii $ Corners 4.0 true false false true
           ][]

         , linearLayout
           [ height $ V 4
           , width $ V (HU.clampNumber 29.0 100.0 ((screenWidth unit) - 96))
           , background Color.black700
           , cornerRadii $ Corners 4.0 false true true false 
           ][]
        ]
     ]
   , linearLayout
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , padding $ Padding 16 12 16 12
     , background Color.blue600
     , cornerRadius 8.0
     , orientation VERTICAL
     , gravity CENTER_VERTICAL
     ][ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , onClick push $ const $ ToggleDueDetails
        ] [ textView
             [ text "Due Details"
             , textSize FontSize.a_12
             , fontStyle $ FontStyle.medium LanguageStyle
             , color Color.black800
             , weight 1.0
             ]
           , imageView
             [ width $ V 16
             , height $ V 16
             , margin (MarginRight 4)
             , imageWithFallback if state.props.myPlanProps.isDuesExpanded 
                                 then "ny_ic_chevron_up,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_up.png"
                                 else "ny_ic_chevron_down,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_down.png"
             ]
          ]
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation HORIZONTAL
          , margin $ MarginVertical 16 8
          , visibility if state.props.myPlanProps.isDuesExpanded then VISIBLE else GONE
          ][ textView
             [ text "Trip Date"
             , textSize FontSize.a_12
             , fontStyle $ FontStyle.medium LanguageStyle
             , color Color.black600
             , weight 1.0
             ]
           , textView
             [ text "Amount"
             , textSize FontSize.a_12
             , fontStyle $ FontStyle.medium LanguageStyle
             , color Color.black600
             ]
          ]
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , margin $ MarginBottom 16
          , visibility if state.props.myPlanProps.isDuesExpanded then VISIBLE else GONE
          ] (map
              (\item -> 
              linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation HORIZONTAL
              , margin $ MarginBottom 8
              ][ textView
                 [ text item.tripDate
                 , textSize FontSize.a_12
                 , fontStyle $ FontStyle.medium LanguageStyle
                 , color Color.black600
                 , weight 1.0
                 ]
               , textView
                 [ text item.amount
                 , textSize FontSize.a_12
                 , fontStyle $ FontStyle.medium LanguageStyle
                 , color Color.black600
                 ]
              ]
              ) state.data.myPlanData.dueItems)
        , linearLayout
          [ height $ V 4
          , width MATCH_PARENT
          , color Color.white900
          , visibility if state.props.myPlanProps.isDuesExpanded then VISIBLE else GONE
          ][]
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER
          , visibility if state.props.myPlanProps.isDuesExpanded then VISIBLE else GONE
          ][textView
            [ textFromHtml "<u>View Due Details</u>"
            , color Color.black650
            , textSize FontSize.a_14
            , fontStyle $ FontStyle.medium LanguageStyle
            , padding $ PaddingBottom 3
            ] 
           ]
      ] 
   , if false then PrimaryButton.view (push <<< ClearDue) (clearDueButtonConfig state) else dummyView
   , if false then arrowButtonView push "Setup Autopay" false NoAction else dummyView
  ]

promoCodeView :: forall w. (Action -> Effect Unit) -> PromoConfig -> PrestoDOM (Effect Unit) w 
promoCodeView push state =
  linearLayout
  ([ height WRAP_CONTENT
  , width WRAP_CONTENT
  , cornerRadius 100.0
  , padding $ Padding 10 4 10 4
  , stroke $ "1," <> Color.grey900
  , background Color.white900
  , margin $ MarginRight 4
  , gravity CENTER_VERTICAL
  ]<> if state.isGradient then [gradient (Linear 90.0 state.gradient)] else [])
   [ imageView
     [ width $ V 12
     , height $ V 12
     , margin (MarginRight 4)
     , visibility if state.hasImage then VISIBLE else GONE
     , imageWithFallback state.imageURL
     ] 
   , textView
     [ text state.title
     , textSize FontSize.a_10
     , fontStyle $ FontStyle.medium LanguageStyle
     , color Color.blue900
     , padding $ PaddingBottom 3
     ]
  ]

alertView :: forall w. (Action -> Effect Unit) -> String -> String -> String -> String -> String -> Action -> PrestoDOM (Effect Unit) w
alertView push image primaryColor title description buttonText action = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , background Color.white900
  , stroke $ "1," <> Color.grey900
  , padding $ Padding 16 16 16 16
  , margin $ Margin 16 16 16 0
  , cornerRadius 8.0
  , orientation VERTICAL
  ][ linearLayout
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , margin $ MarginBottom 4
     , gravity CENTER_VERTICAL
     ][ imageView
        [ width $ V 16
        , height $ V 16
        , margin $ MarginRight 4
        , imageWithFallback image
        ]
      , textView
        [ text title
        , textSize FontSize.a_14
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , color primaryColor
        , padding $ PaddingBottom 3
        ]
      ] 
   , textView
     [ text description
     , textSize FontSize.a_12
     , fontStyle $ FontStyle.medium LanguageStyle
     , color Color.black600
     , margin $ if buttonText /= "" then MarginBottom 12 else MarginBottom 0
     ]
   , if buttonText /= "" then arrowButtonView push buttonText true action else dummyView
  ]

arrowButtonView :: forall w. (Action -> Effect Unit) -> String -> Boolean -> Action -> PrestoDOM (Effect Unit) w
arrowButtonView push title arrowVisibility action = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER
  ][ linearLayout
     [ height WRAP_CONTENT
     , width WRAP_CONTENT
     , gravity CENTER_VERTICAL
     , onClick push $ const $ action
     ][ textView
        [ text title
        , textSize FontSize.a_14
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , color Color.blue800
        , margin (MarginRight 4)
        , padding $ PaddingBottom 3
        ]
      , imageView
        [ width $ V 18
        , height $ V 18
        , visibility if arrowVisibility then VISIBLE else GONE
        , imageWithFallback "ny_ic_arrow_right_blue,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_arrow_right_blue.png"
        ]
     ]
  ]

paymentMethodView :: forall w. (Action -> Effect Unit) -> MyPlanData -> PrestoDOM (Effect Unit) w
paymentMethodView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  , cornerRadius 100.0
  , background Color.grey700
  , padding $ Padding 8 5 8 5
  ][ imageView
    [ width $ V 12
    , height $ V 12
    , margin (MarginRight 4)
    , visibility if state.paymentMethod == UPI_AUTOPAY then VISIBLE else GONE
    , imageWithFallback (getImageURL "ny_ic_upi_logo")
    ]
  , textView 
    [ text if state.paymentMethod == UPI_AUTOPAY then "UPI Autopay" else "Manual Payment"
    , textSize FontSize.a_10
    , fontStyle $ FontStyle.medium LanguageStyle
    , color Color.black900
    , padding $ PaddingBottom 3
    ]
  , linearLayout
    [ height $ V 4
    , width $ V 4
    , background if state.autoPayStatus == AUTOPAY_ACTIVE then Color.green900 else Color.orange900
    , cornerRadius 12.0
    , visibility if state.paymentMethod == UPI_AUTOPAY then VISIBLE else GONE
    , margin $ MarginHorizontal 4 4
    ][]
  , textView
    [ text if state.autoPayStatus == AUTOPAY_ACTIVE then "Active" else "Paused"
    , textSize FontSize.a_10
    , visibility if state.paymentMethod == UPI_AUTOPAY then VISIBLE else GONE
    , fontStyle $ FontStyle.medium LanguageStyle
    , color if state.autoPayStatus == AUTOPAY_ACTIVE then Color.green900 else Color.orange900
    , padding $ PaddingBottom 3
    ]
  ]

managePlanBodyView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
managePlanBodyView push state =
  scrollView
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , scrollBarY false
  , padding $ PaddingVertical 55 45
  ][ linearLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , padding $ PaddingTop 24
     , margin $ MarginHorizontal 16 16
     , orientation VERTICAL
     ][ textView
        [ text "CURRENT PLAN"
        , textSize FontSize.a_12
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , color Color.black700
        , margin $ MarginBottom 12
        ]
      , planCardView push state.data.managePlanData.currentPlan (state.data.managePlanData.currentPlan.id == state.props.managePlanProps.selectedPlan) SelectPlan
      , textView
        [ text "ALTERNATE PLAN"
        , textSize FontSize.a_12
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , color Color.black700
        , margin $ MarginVertical 32 12 
        ]
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ](map(
             (\item -> planCardView push item (item.id == state.props.managePlanProps.selectedPlan) SelectPlan)
             ) state.data.managePlanData.alternatePlans)
      , PrimaryButton.view (push <<< SwitchPlan) (switchPlanButtonConfig state)
     ]
   ]

planCardView :: forall w. (Action -> Effect Unit) -> PlanCardConfig -> Boolean -> (String -> Action)-> PrestoDOM (Effect Unit) w
planCardView push state isSelected action =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , background if isSelected then Color.blue600 else Color.white900
  , stroke $ "1," <> (if isSelected then Color.blue800 else Color.grey900)
  , padding $ Padding 16 12 16 12
  , cornerRadius 8.0
  , orientation VERTICAL
  , margin $ MarginBottom 16
  , onClick push $ const $ action state.id
  ][ linearLayout
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , gravity CENTER_VERTICAL
     , margin $ MarginBottom 5
     ][ textView
        [ text state.title
        , textSize FontSize.a_14
        , weight 1.0
        , fontStyle $ (if isSelected then FontStyle.bold else FontStyle.semiBold) LanguageStyle
        , color if isSelected then Color.blue900 else Color.black700
        ]
      , textView
        [ text $ "₹" <> (show state.planPrice) <> "/ Ride"
        , textSize FontSize.a_16
        , fontStyle $ FontStyle.bold LanguageStyle
        , color Color.black800
        ]
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      ][ textView
         [ text state.description
         , textSize FontSize.a_12
         , fontStyle $ FontStyle.medium LanguageStyle
         , color Color.black600
         , weight 1.0
         ]
       , offerCountView (DA.length state.offers) isSelected
       ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , visibility if isSelected && (DA.length state.offers > 0) then VISIBLE else GONE
      , margin $ MarginVertical 12 12
      ](map 
          (\item -> promoCodeView push item
          ) state.offers)
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , padding $ Padding 8 8 8 8
      , background Color.white900
      , visibility if isSelected && (DS.length state.offerDescription > 0) then VISIBLE else GONE
      , cornerRadius 4.0
      ][ textView
        [ textFromHtml state.offerDescription
        , textSize FontSize.a_12
        , fontStyle $ FontStyle.regular LanguageStyle
        , color Color.black600
        , lineHeight "20"
        ]
      ]
   ]

offerCountView :: forall w. Int -> Boolean -> PrestoDOM (Effect Unit) w
offerCountView count isSelected = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , cornerRadius 100.0
  , stroke $ "1," <> Color.blue700
  , background Color.blue600
  , padding $ Padding 10 2 10 2
  , visibility if (count > 0 && not isSelected) then VISIBLE else GONE
  , gravity CENTER_VERTICAL
  ][ imageView
     [ imageWithFallback $ getImageURL "ny_ic_discount"
     , width $ V 12
     , height $ V 12
     , margin $ MarginRight 4
     ]
   , textView
     [ text $ "+" <> show count <> " " <> "Offers"
     , textSize FontSize.a_10
     , fontStyle $ FontStyle.semiBold LanguageStyle
     , color Color.blue900
     , padding $ PaddingBottom 3
     ]
  ]

dummyView :: forall w. PrestoDOM (Effect Unit) w
dummyView = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  ][]

getImageURL :: String -> String
getImageURL imageName = imageName <> "," <> (getAssetStoreLink FunctionCall) <> ".png"

autoPayDetailsView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> Boolean -> PrestoDOM (Effect Unit) w
autoPayDetailsView push state visibility' = 
  PrestoAnim.animationSet [ Anim.fadeIn visibility' ] $
  relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , visibility if visibility' then VISIBLE else GONE
  ][ linearLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , orientation VERTICAL
     ][ headerView push "Autopay Details" "" true
      , autoPayPGView push state
      , scrollView
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , scrollBarY false
        , margin $ Margin 20 16 20 16
        , cornerRadius 24.0
        ][ linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , orientation VERTICAL
            ] (DA.mapWithIndex (\index item -> 
              linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              ][ linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , background Color.blue600
                  , padding $ Padding 16 8 16 8
                  ][ commonTV push item.key Color.black700 (FontStyle.body3 TypoGraphy) 0 LEFT
                  , linearLayout
                      [ weight 1.0
                      , height WRAP_CONTENT
                      , gravity RIGHT
                      ][ commonTV push item.val Color.black900 (FontStyle.body6 TypoGraphy) 0 RIGHT ]
                  ]
                , linearLayout
                  [ width MATCH_PARENT
                  , height $ V 1
                  , background Color.white900
                  , visibility if index == (DA.length state.data.autoPayDetails.detailsList -1) then GONE else VISIBLE
                  ][]
              ]
              ) state.data.autoPayDetails.detailsList)
    ]
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , alignParentBottom "true,-1"
      , background Color.grey900
      , padding $ Padding 5 5 5 5
      , gravity CENTER
      ][ textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , textFromHtml "<u>Cancel Autopay</u>"
          , color Color.black800
          , padding $ Padding 5 5 5 5
          , onClick push $ const CancelAutoPayAC
          ] <> FontStyle.body3 TypoGraphy
      ]
   ]

autoPayPGView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
autoPayPGView push state = 
  relativeLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ Margin 20 22 20 22
  ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      ][ imageView
          [ imageWithFallback "ny_ic_phonepe,"
          , height $ V 45
          , width $ V 45
          ]
        , linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          , margin $ MarginLeft 10
          ][ linearLayout
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , gravity CENTER_VERTICAL
              ][ commonTV push "UPI Autopay" Color.black800 (FontStyle.body1 TypoGraphy) 0 LEFT
               , imageView
                  [ imageWithFallback "ny_ic_upi_logo,"
                  , height $ V 14
                  , width $ V 14
                  ]
              ]
            , commonTV push "Account: HDFC ******7881" Color.black800 (FontStyle.body1 TypoGraphy) 0 LEFT
          ]
        , linearLayout
          [ height WRAP_CONTENT
          , weight 1.0
          ][]
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity RIGHT
      ][ textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity RIGHT
          , cornerRadius 20.0
          , padding $ Padding 7 7 7 7
          , background Color.green600
          , color Color.green900
          , text "Success"
          ] <> FontStyle.tags TypoGraphy
      ]
  ]