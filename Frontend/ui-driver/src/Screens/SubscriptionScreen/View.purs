module Screens.SubscriptionScreen.View where

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.BottomNavBar (navData)
import Components.BottomNavBar as BottomNavBar
import Components.PrimaryButton as PrimaryButton
import Data.String as DS
import Debug (spy)
import Effect (Effect)
import Engineering.Helpers.Commons (screenWidth)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils as HU
import Prelude (Unit, const, map, ($), (<<<), (<>), (-), (>), (&&), unit)
import PrestoDOM (Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), afterRender, alignParentBottom, background, color, cornerRadius, fontStyle, gradient, gravity, height, imageView, imageWithFallback, lineHeight, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, scrollBarY, scrollView, stroke, text, textFromHtml, textSize, textView, visibility, weight, width)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens as ScreenNames
import Screens.SubscriptionScreen.ComponentConfig (clearDueButtonConfig, joinPlanButtonConfig, switchPlanButtonConfig)
import Screens.SubscriptionScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.SubscriptionScreen.ScreenData (dummyPlanConfig)
import Screens.Types (SubscriptionScreenState, SubscriptionSubview(..), PromoConfig, PlanCardConfig)
import Styles.Colors as Color


screen :: SubscriptionScreenState -> Screen Action SubscriptionScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "SubscriptionScreen"
  , globalEvents: []
  , eval:
      ( \state action -> do
          let _ = spy "SubscriptionScreen ----- state" state
          let _ = spy "SubscriptionScreen --------action" action
          eval state action
      )
  }


view :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  , onBackPressed push $ const BackPressed
  , afterRender push $ const AfterRender
  , background Color.white900
  ][ Anim.screenAnimationFadeInOut $
      linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , weight 1.0
      ][ case state.props.subView of 
          JoinPlan -> joinPlanView push state
          ManagePlan -> managePlanView push state
          MyPlan -> myPlanView push state
      ]
    , BottomNavBar.view (push <<< BottomNavBarAction) (navData ScreenNames.SUBSCRIPTION_SCREEN)
  ]


joinPlanView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
joinPlanView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
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
              , text "How it works?"
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
              ](map (\item -> 
                  planCardView push dummyPlanConfig false
                  ) [1,2])
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


managePlanView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
managePlanView push state =
  relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
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
        , fontStyle $ FontStyle.regular LanguageStyle
        , color Color.black800
        ]
      ]
   ]


myPlanView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
myPlanView push state =
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , gradient (Linear 180.0 ["#E2EAFF", "#F5F8FF"])
  ][ headerView push "Plan" "<u>How it works?</u>" false
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
       , margin $ MarginHorizontal 16 16 
       ][ textView 
           [ text "My Plan"
           , textSize FontSize.a_20
           , fontStyle $ FontStyle.bold LanguageStyle
           , color Color.black800
           , weight 1.0
           ]
         , linearLayout
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
              , imageWithFallback "ny_ic_upi_logo,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_upi_logo.png"
              ]
            , textView 
              [ text "UPI Autopay"
              , textSize FontSize.a_10
              , fontStyle $ FontStyle.medium LanguageStyle
              , color Color.black900
              , padding $ PaddingBottom 3
              ]
            , linearLayout
              [ height $ V 4
              , width $ V 4
              , background Color.green900
              , cornerRadius 12.0
              , margin $ MarginHorizontal 4 4
              ][]
            , textView
              [ text "Active"
              , textSize FontSize.a_10
              , fontStyle $ FontStyle.medium LanguageStyle
              , color Color.green900
              , padding $ PaddingBottom 3
              ]
           ]
       ]
     , planDescriptionView push state
     , if false then lowBalanceView push state else dummyView push state
     , if false then switchAndSaveView push state else dummyView push state
     , duesView push state
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
  , arrowButtonView push "View Plan Details" true NoAction
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
      ]
   , if false then PrimaryButton.view (push <<< ClearDue) (clearDueButtonConfig state) else dummyView push state
   , if false then arrowButtonView push "Setup Autopay" false NoAction else dummyView push state
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

lowBalanceView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
lowBalanceView push state = 
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
     , margin $ MarginBottom 8
     , gravity CENTER_VERTICAL
     ][ imageView
        [ width $ V 16
        , height $ V 16
        , margin $ MarginRight 4
        , imageWithFallback "ny_ic_warning_red,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_warning_red.png"
        ]
      , textView
        [ text "Low Account Balance"
        , textSize FontSize.a_14
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , color Color.red
        , padding $ PaddingBottom 3
        ]
      ] 
   , textView
     [ text "Your bank account balance is low. Add at least ₹25 by 17 Aug 2023 to enjoy uninterrupted rides."
     , textSize FontSize.a_12
     , fontStyle $ FontStyle.medium LanguageStyle
     , color Color.black600
     ]
  ]

switchAndSaveView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
switchAndSaveView push state = 
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
     , margin $ MarginBottom 8
     , gravity CENTER_VERTICAL
     ][ imageView
        [ width $ V 16
        , height $ V 16
        , margin $ MarginRight 4
        , imageWithFallback "ny_ic_warning_blue,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_warning_blue.png"
        ]
      , textView
        [ text "Switch and Save"
        , textSize FontSize.a_14
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , color Color.blue800
        , padding $ PaddingBottom 3
        ]
      ] 
   , textView
     [ text "You have completed over 7 rides today. Save up to ₹10 by switching to the DAILY UNLIMITED plan"
     , textSize FontSize.a_12
     , fontStyle $ FontStyle.medium LanguageStyle
     , color Color.black600
     , margin $ MarginBottom 12
     ]
   , arrowButtonView push "Switch Now" true NoAction
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
      , planCardView push dummyPlanConfig true
      , textView
        [ text "ALTERNATE PLAN"
        , textSize FontSize.a_12
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , color Color.black700
        , margin $ MarginVertical 32 12 
        ]
      , planCardView push dummyPlanConfig false
      , PrimaryButton.view (push <<< SwitchPlan) (switchPlanButtonConfig state)
     ]
   ]

planCardView :: forall w. (Action -> Effect Unit) -> PlanCardConfig -> Boolean -> PrestoDOM (Effect Unit) w
planCardView push state isSelected =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , background if isSelected then Color.blue600 else Color.white900
  , stroke $ "1," <> (if isSelected then Color.blue800 else Color.grey900)
  , padding $ Padding 16 12 16 12
  , cornerRadius 8.0
  , orientation VERTICAL
  ][ linearLayout
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , gravity CENTER_VERTICAL
     , margin $ MarginBottom 6
     ][ textView
        [ text "DAILY PER TRIP"
        , textSize FontSize.a_14
        , weight 1.0
        , fontStyle $ (if isSelected then FontStyle.bold else FontStyle.semiBold) LanguageStyle
        , color if isSelected then Color.blue900 else Color.black700
        ]
      , textView
        [ text "₹3.5/ Trip"
        , textSize FontSize.a_16
        , fontStyle $ FontStyle.bold LanguageStyle
        , color Color.black800
        ]
      ]
    , textView
      [ text "Up to a maximum of ₹35 per day"
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
        [ textFromHtml "Freedom offer : <b> ₹6/Day from Sep 1-30 </b> <br>Valid only if you join by <b> Aug 16. </b> <br> <b> No charges till Aug 31 </b>"
        , textSize FontSize.a_12
        , fontStyle $ FontStyle.regular LanguageStyle
        , color Color.black600
        , lineHeight "20"
        ]
      ]
   ]


dummyView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
dummyView _ _ = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  ][]