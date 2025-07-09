module Screens.DriverClaimRewardScreen.View where

import Prelude (Unit, const, map, not, ($), (<<<), (<>), (==), (>), show, (<=), pure,(>=),(<),unit,(-),(&&))
import Effect (Effect)
import Data.Array (length, take, drop, mapWithIndex,(!!))
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), rippleColor,PrestoDOM, LoggableScreen, Visibility(..),scrollView, scrollBarY, afterRender, alpha, background, color, cornerRadius, fontStyle,relativeLayout, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, stroke, text, textSize, textView, weight, width, frameLayout, visibility, clickable, singleLine, id,alignParentBottom,textFromHtml)
import Screens.DriverClaimRewardScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Debug
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Function.Uncurried (runFn5)
import Common.Types.App (LazyCheck(..))
import Common.Types.App (YoutubeData(..), YoutubeVideoStatus(..))
import Engineering.Helpers.Commons (getNewIDWithTag)
import JBridge (setYoutubePlayer, openUrlInApp)
import Data.Int (toNumber)

type Benefit = { icon :: String, label :: String, description :: String }
type FAQ = { question :: String, answer :: String }

screen :: ST.DriverClaimRewardScreenState -> LoggableScreen Action ST.DriverClaimRewardScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "DriverClaimRewardScreen"
  , globalEvents: []
  , eval:
      ( \state action -> do
          let _ = spy "DriverClaimRewardScreenState -----" state
          let _ = spy "DriverClaimRewardScreenState--------action" action
          eval state action
      )
  , parent : Nothing
  , logWhitelist : []
  }

view :: forall w. (Action -> Effect Unit) -> ST.DriverClaimRewardScreenState -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , onBackPressed push $ const BackPressed
    , background Color.white900
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        ]
        [ headerLayout push state
        , if state.props.showFaq then faqQuestionView push state else 
          linearLayout
            [ width MATCH_PARENT
            , weight 1.0
            , orientation VERTICAL
            ]
            [ scrollView
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , scrollBarY false
                ]
                [ scrollableContent push state
                ]
            ]
        ]
    , claimButton push state
    ]

headerLayout :: forall w. (Action -> Effect Unit) -> ST.DriverClaimRewardScreenState -> PrestoDOM (Effect Unit) w
headerLayout push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background Color.blue600
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation HORIZONTAL
        , layoutGravity "center_vertical"
        , padding $ PaddingVertical 20 20
        ]
        [ imageView
            [ width $ V 30
            , height $ V 30
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
            , gravity CENTER_VERTICAL
            , onClick push $ const BackPressed
            , padding $ Padding 2 2 2 2
            , margin $ MarginLeft 5
            ]
        , linearLayout [weight 1.0] []
        , imageView 
            [ height $ V 25
            , width $ V 25
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_phone_filled_blue"
            , margin $ Margin 0 5 25 0
            , onClick push $ const CallSupport
            ]
        , linearLayout 
            [ width WRAP_CONTENT
            , height MATCH_PARENT
            , gravity CENTER_VERTICAL
            , cornerRadius 30.0
            , stroke ("1," <> Color.blue700)
            , padding $ Padding 10 2 10 2
            , margin $ MarginRight 15
            , visibility $ if not state.props.showFaq then VISIBLE else GONE
            , onClick push $ const FaqClicked
            ]
            [ textView 
                [ text "FAQs"
                , color Color.blue700
                ]
            ]
        ]
    ]

scrollableContent :: forall w. (Action -> Effect Unit) -> ST.DriverClaimRewardScreenState -> PrestoDOM (Effect Unit) w
scrollableContent push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ nammaKutumbaView push state
    , nominationView push state
    , infoCardForDriverTag push state
    , infoCardForNonEligibility push state
    , benefitsSection push state
    , eligibilitySection push state
    , termsAndConditionsButton push state
    , linearLayout [ height $ V 100 ] []
    ]

nammaKutumbaView :: forall w. (Action -> Effect Unit) -> ST.DriverClaimRewardScreenState -> PrestoDOM (Effect Unit) w
nammaKutumbaView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.blue600
    , padding $ Padding 20 5 20 16
    ]
    [ imageView $
        [ width $ V 90
        , height $ V 90
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_namma_kutumba"
        , layoutGravity "center_horizontal"
        , margin $ MarginBottom 10
        ]
    , textView $
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , text "Namma Kutumba"
        , color Color.black800
        , gravity CENTER
        , margin $ MarginBottom 10
        ] <> FontStyle.h1 TypoGraphy
    , youtubeVideoView push state
    ]

youtubeVideoView :: forall w. (Action -> Effect Unit) -> ST.DriverClaimRewardScreenState -> PrestoDOM (Effect Unit) w
youtubeVideoView push state =
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity CENTER
      , padding $ Padding 20 16 15 16
      , cornerRadius 16.0
      , margin $ MarginBottom 4
      , id $ getNewIDWithTag "nammaKutumbaVideo"
      , afterRender
          ( \action -> do
              let id = getNewIDWithTag "nammaKutumbaVideo"
              pure $ runFn5 setYoutubePlayer (youtubeData state "VIDEO") id (show ST.PLAY) push YoutubeVideoStatus
          )
          (const NoAction)
      ]
      []

youtubeData :: ST.DriverClaimRewardScreenState -> String -> YoutubeData
youtubeData state mediaType =
  { videoTitle: "title"
  , setVideoTitle: false
  , showMenuButton: false
  , showDuration: true
  , showSeekBar: true
  , videoId: "QYjybDBNxHc"
  , videoType: "VIDEO"
  , videoHeight: 200
  , showFullScreen: false
  , hideFullScreenButton : false
  }

nominationView :: forall w. (Action -> Effect Unit) -> ST.DriverClaimRewardScreenState -> PrestoDOM (Effect Unit) w
nominationView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background "#F6E6FF" 
    , cornerRadius 12.0
    , visibility $ if state.props.showNominationView then VISIBLE else GONE
    , margin $ Margin 16 16 16 0
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        ]
        [ textView
            [ text "Nominate your Representative"
            , color Color.black900
            , weight 1.0
            , textSize FontSize.a_18
            , padding $ Padding 16 0 0 0
            ]
        , imageView
            [ width $ V 27
            , height $ V 27
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right_purple" 
            , gravity CENTER
            , margin $ Margin 0 20 15 7
            ]
        ]
    , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , padding $ Padding 16 0 0 10
        , gravity LEFT
        , onClick (\_ -> openUrlInApp $ "https://www.youtube.com/@nammayatri") (const unit)
        ]
        [ textView
            [ textFromHtml "<u>Watch video</u>"
            , color "#1976D2" 
            , textSize FontSize.a_16
            ]
        , imageView
            [ width $ V 24
            , height $ V 18
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_youtube"
            , margin $ MarginLeft 6
            , margin $ MarginTop 2
            ]
        ]
    ]

infoCardForDriverTag :: forall w. (Action -> Effect Unit) -> ST.DriverClaimRewardScreenState -> PrestoDOM (Effect Unit) w
infoCardForDriverTag push state =
  let driverTag = state.data.driverTag
      info =
        case driverTag of
          "ny_member_probation"   -> { label: "You are in probation period till 12/07/2025\nPass all Eligibility Criteria to continue being a member", color: "#FFEBDD", visible: true, support: false }
          "ny_member_revoked"     -> { label: "Your Namma Kutumba Membership has been revoked", color: "#FFE9E9", visible: true, support: true }
          _ -> { label: "", color: "#FFFFFF", visible: false, support: false }
  in
    infoCard info.label info.color info.visible info.support push

infoCardForNonEligibility :: forall w. (Action -> Effect Unit) -> ST.DriverClaimRewardScreenState -> PrestoDOM (Effect Unit) w
infoCardForNonEligibility push state =
  let info = { label: "High cancellation rate. Take more rides to continue being a part of Namma Kutumba", color: "#FFF6E0", visible: (fromMaybe 0 state.data.cancellationRateInWindow) >= 30 , support: false }
  in
    infoCard info.label info.color info.visible info.support push

infoCard :: forall w. String -> String -> Boolean -> Boolean -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
infoCard label backgroundColor visible showSupport push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background backgroundColor
    , cornerRadius 16.0
    , margin $ Margin 12 10 12 10
    , padding $ Padding 12 16 12 16
    , orientation VERTICAL
    , visibility $ if visible then VISIBLE else GONE
    ]
    ([ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        ]
        [ imageView
            [ width $ V 18
            , height $ V 18
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_black"
            , margin $ MarginRight 8
            ]
         , textView
            [ text label
            , color "#222222"
            , weight 1.0
            ]
         ]
     ]
     <> if showSupport then
          [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , margin $ MarginTop 10
            , gravity CENTER_VERTICAL
            ]
            [ imageView
                [ width $ V 16
                , height $ V 16
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_phone_filled_blue"
                , margin $ MarginRight 8
                , onClick push $ const CallSupport
                ]
            , textView
                [ text "Call Support"
                , color Color.blue900
                , onClick push $ const OpenWhatsAppSupport
                , margin $ MarginTop 8
                , margin $ MarginLeft 6
                ]
            ]
        ]
        else []
    )

benefits :: Array Benefit
benefits =
  [ { icon: "ny_ic_medical_support"
    , label: "Accidental Medical Support"
    , description: "Receive coverage of up to ₹20,000 per incident, claimable once every year."
    }
  , { icon: "ny_ic_death_support"
    , label: "Accidental Death Support"
    , description: "If an accidental death occurs, your family is eligible to claim ₹50,000 as compensation."
    }
  , { icon: "ny_ic_health_insurance"
    , label: "Medical Health Insurance"
    , description: "Opt in to a low-cost medical insurance plan designed for drivers like you. Affordable, optional, and easy to enroll."
    }
  , { icon: "ny_ic_personal_loans"
    , label: "Personal Loans"
    , description: "Avail personal loans through our lending partners at preferential interest rates, subject to terms and eligibility."
    }
  , { icon: "ny_ic_namma_support"
    , label: "Namma Support"
    , description: "Receive professional assistance for challenges related to RTO procedures, legal issues, and other administrative matters."
    }
  ]

benefitsSection :: forall w. (Action -> Effect Unit) -> ST.DriverClaimRewardScreenState -> PrestoDOM (Effect Unit) w
benefitsSection push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.white900
    ]
    [ textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text "Benefits"
        , color Color.black900
        , padding $ Padding 16 20 0 10
        ] <> FontStyle.h2 TypoGraphy
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        (mapWithIndex (\i b -> benefitItem push i state b) (if state.props.showAllBenefits then benefits else take 5 benefits) <>
         if length benefits > 5 && not state.props.showAllBenefits
           then [viewMoreButton push ViewMoreBenefits state.props.showAllBenefits]
           else [])
    ]

benefitItem :: forall w. (Action -> Effect Unit) -> Int -> ST.DriverClaimRewardScreenState -> Benefit -> PrestoDOM (Effect Unit) w
benefitItem push i state benefit =
  let isOpen = state.props.openBenefitIndex == Just i
  in linearLayout
    [ width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , cornerRadius 12.0
    , margin $ MarginBottom 8
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        , padding $ Padding 16 8 12 8
        , onClick push $ const (BenefitToggle i)
        ]
        [ imageView
            [ width $ V 24
            , height $ V 24
            , imageWithFallback $ fetchImage FF_ASSET benefit.icon
            , margin $ MarginRight 12
            ]
        , textView $
            [ text benefit.label
            , color Color.black900
            , weight 1.0
            ] <> FontStyle.body25 TypoGraphy
        , imageView
            [ width $ V 20
            , height $ V 20
            , imageWithFallback $ fetchImage FF_ASSET $ if isOpen then "ny_ic_chevron_up_dark" else "ny_ic_chevron_right_black"
            ]
        ]
    , if isOpen then
        linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , background Color.white900
          , padding $ Padding 16 0 16 5
          ]
          [ textView $
              [ text benefit.description
              , color "#7B7B8B"
              , textSize FontSize.a_15
              ]
          ]
      else
        linearLayout [] []
    ]

eligibilityArray :: ST.DriverClaimRewardScreenState -> Array { label :: String, visibility :: Boolean }
eligibilityArray state =
  [{ label:"Completed 50+ rides in the last 3 months" , visibility: (fromMaybe 0 state.data.numberOfRides) >= 50}
  , { label:"Safety score of 80% or above" , visibility: (fromMaybe 0 state.data.safetyScore) >= 80}
  , { label:"Rating of 4.6 or above" , visibility: (fromMaybe 0.0 state.data.rating) >= 4.6}
  , { label:"Cancellation rate below 30%" , visibility: (fromMaybe 0 state.data.cancellationRateInWindow) < 30}
  ]

eligibilitySection :: forall w. (Action -> Effect Unit) -> ST.DriverClaimRewardScreenState -> PrestoDOM (Effect Unit) w
eligibilitySection push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.white900
    ]
    [ textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text "Eligibility Criteria"
        , color Color.black900
        , padding $ Padding 16 20 0 10
        ] <> FontStyle.h2 TypoGraphy
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        (map (eligibilityItem push) (getEligibilityToShow state) <>
         if shouldShowViewMore (map (\x -> x.visibility) (eligibilityArray state)) state.props.showAllEligibility
           then [viewMoreButton push ViewMoreEligibility state.props.showAllEligibility]
           else [])
    ]

eligibilityItem :: forall w. (Action -> Effect Unit) -> { label :: String, visibility :: Boolean } -> PrestoDOM (Effect Unit) w
eligibilityItem push criterion =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ Padding 16 8 12 0
    , margin $ MarginBottom 8
    ]
    [ imageView
        [ width $ V 16
        , height $ V 16
        , imageWithFallback $ fetchImage FF_ASSET ( if criterion.visibility then "ny_ic_green_tick" else "ny_ic_pending")
        , margin $ MarginTop 4
        ]
    , textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text criterion.label
        , margin $ MarginLeft 10
        , color Color.black900
        , weight 1.0
        ] <> FontStyle.body25 TypoGraphy
    ]

faqs :: Array FAQ
faqs =
  [ { question: "What is Kutumba?"
    , answer: "Kutumba (Namma Driver Welfare Trust) is an initiative / program by Namma Yatri to promote the welfare of drivers associated with Namma Yatri, including financial support, health benefits, rewards, and recognition programs."
    }
  , { question: "How can I be eligible?"
    , answer: "The eligibility to the Kutumba can be obtained by following 3 simple steps; Ride More, Cancel Less and Ensure Good Customer Service."
    }
  , { question: "Can I refer a friend to Kutumba once I am eligible?"
    , answer: "No. One cannot refer to the Kutumba. The eligibility can be earned by following what is explained above."
    }
  , { question: "How do I claim a benefit?"
    , answer: "At the time of claiming a benefit, you need to be a part of the Kutumba. You can claim a benefit from the Kutumba Section (a sub-section in the Benefits section) of the Namma Yatri Partner App."
    }
  , { question: "Are there limits on claims per month?"
    , answer: "The disbursal of the amount against the claims made is decided by the working committee as per the budget allocated in the particular month. The unutilised budget will be carried over to the subsequent month."
    }
  , { question: "Whom do I contact for queries?"
    , answer: "For more queries, please contact: +91-xxxxxxxxxx"
    }
  ]

faqQuestionView :: forall w. (Action -> Effect Unit) -> ST.DriverClaimRewardScreenState -> PrestoDOM (Effect Unit) w
faqQuestionView push state =
  scrollView
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background "#F6F8FC"
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , background Color.white900
        , cornerRadius 16.0
        , margin $ Margin 16 16 16 16
        , orientation VERTICAL
        ]
        (mapWithIndex (\i faq ->
          let isOpen = state.props.openFaqIndex == Just i
          in linearLayout
            [ width MATCH_PARENT
            , orientation VERTICAL
            ]
            [ linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation HORIZONTAL
                , gravity CENTER_VERTICAL
                , padding $ Padding 16 16 16 16
                , onClick push $ const (FaqToggle i)
                , rippleColor Color.rippleShade
                ]
                [ textView $
                    [ text faq.question
                    , color Color.black900
                    , weight 1.0
                    , margin $ MarginRight 8
                    ] <> FontStyle.subHeading2 TypoGraphy
                , imageView
                    [ width $ V 25
                    , height $ V 25
                    , imageWithFallback $ fetchImage FF_ASSET $ if isOpen then "ny_ic_chevron_up_dark" else "ny_ic_chevron_right_black"
                    , onClick push $ const (FaqToggle i)
                    ]
                ]
            , if isOpen then
                linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , padding $ Padding 16 0 16 16
                  , background "#F9F9F9"
                  ]
                  [ textView $
                      [ text faq.answer
                      , color "#555555"
                      ] <> FontStyle.body3 TypoGraphy
                  ]
              else
                linearLayout [] []
            , if i == (length faqs - 1) then
                linearLayout [] []
              else
                linearLayout
                  [ width MATCH_PARENT
                  , height $ V 1
                  , background "#E0E0E0"
                  ]
                  []
            ]
        ) faqs)
    ]

termsAndConditionsButton :: forall w. (Action -> Effect Unit) -> ST.DriverClaimRewardScreenState -> PrestoDOM (Effect Unit) w
termsAndConditionsButton push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , background "#F6F7F9"
    , cornerRadius 16.0
    , margin $ Margin 16 24 16 16
    , padding $ Padding 20 18 20 18
    , gravity CENTER_VERTICAL
    , onClick (\_ -> openUrlInApp $ "https://docs.google.com/document/d/1K68xvtReD9FVpx-IshtKNMt4baQNgKXt") (const unit)
    ]
    [ textView
        [ text "Terms & Conditions"
        , color Color.black900
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , textSize FontSize.a_16
        , weight 1.0
        ]
    , imageView
        [ width $ V 20
        , height $ V 20
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right_grey"
        ]
    ]

claimButton :: forall w. (Action -> Effect Unit) -> ST.DriverClaimRewardScreenState -> PrestoDOM (Effect Unit) w
claimButton push state =
  linearLayout
    [ width MATCH_PARENT
    , height $ V 90
    , alignParentBottom "true,-1"
    , background Color.white900
    , visibility $ if not state.props.showFaq then VISIBLE else GONE
    , stroke $ "1," <> Color.borderGreyColor
    , padding $ Padding 16 8 16 16
    , gravity CENTER
  ]
    [ linearLayout
        [ width MATCH_PARENT
        , height $ V 55
        , background Color.black900
        , cornerRadius 12.0
        , margin $ Margin 0 12 0 10
        , gravity CENTER
        , onClick push $ const OpenWhatsAppSupport
        ]
        [ textView
            [ text "Claim via"
            , color Color.yellow900
            , fontStyle $ FontStyle.semiBold LanguageStyle
            , textSize FontSize.a_18
            , gravity CENTER
            ]
        , imageView 
            [ width $ V 75
            , height $ V 18
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_whatsapp_green"
            , margin $ Margin 5 2 0 0
            ]
        ]
    ]

getBenefitsToShow :: ST.DriverClaimRewardScreenState -> Array Benefit
getBenefitsToShow state =
  if state.props.showAllBenefits
    then benefits
    else take 5 benefits

getEligibilityToShow :: ST.DriverClaimRewardScreenState -> Array { label :: String, visibility :: Boolean }
getEligibilityToShow state = 
  if state.props.showAllEligibility
    then eligibilityArray state
    else take 5 (eligibilityArray state)

shouldShowViewMore :: forall a. Array a -> Boolean -> Boolean
shouldShowViewMore arr showingAll = length arr > 5

viewMoreButton :: forall w. (Action -> Effect Unit) -> Action -> Boolean -> PrestoDOM (Effect Unit) w
viewMoreButton push action showingAll =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ Padding 12 8 12 8
    , gravity LEFT
  ]
    [ textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text if showingAll then "View Less" else getString VIEW_MORE
        , color Color.blue900
        , onClick push $ const action
        ] <> FontStyle.body1 TypoGraphy
    , imageView $
        [ width $ V 16
        , height $ V 16
        , imageWithFallback $ fetchImage FF_ASSET ( if showingAll then "ny_ic_chevronup_blue" else "ny_ic_chevrondown_blue")
        , gravity CENTER
        , margin $ MarginTop 3
        ]
    ]
