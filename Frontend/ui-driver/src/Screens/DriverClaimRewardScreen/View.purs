module Screens.DriverClaimRewardScreen.View where

import Prelude (Unit, const, map, not, ($), (<<<), (<>), (==), (>), show, (<=), pure,(>=),(<),unit,(-),(&&), bind, when, void, discard)
import Effect (Effect)
import Data.Array (length, take, drop, mapWithIndex,(!!))
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), rippleColor,PrestoDOM, LoggableScreen, Visibility(..),scrollView, scrollBarY, afterRender, alpha, background, color, cornerRadius, fontStyle,relativeLayout, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, stroke, text, textSize, textView, weight, width, frameLayout, visibility, clickable, singleLine, id,alignParentBottom,textFromHtml)
import Screens.DriverClaimRewardScreen.Controller (Action(..), ScreenOutput, eval, getVideoBannerConfigs)
import Screens.Types as ST
import Styles.Colors as Color
import Helpers.Utils (fetchImage, FetchImageFrom(..), convertEpochToDateFormat)
import Debug
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Function.Uncurried (runFn5)
import Common.Types.App (LazyCheck(..))
import Common.Types.App (YoutubeData(..), YoutubeVideoStatus(..))
import Engineering.Helpers.Commons (getNewIDWithTag, getVideoID)
import JBridge (setYoutubePlayer, openUrlInApp)
import Data.Int (toNumber)
import Resource.Localizable.TypesV2 as LT2
import Resource.Localizable.StringsV2 (getStringV2)
import PrestoDOM.List (ListItem, preComputeListItem)
import CarouselHolder as CarouselHolder
import Components.BannerCarousel as Carousel
import Data.Maybe (isNothing)
import Effect.Aff (launchAff, launchAff_, apathize)
import Engineering.Helpers.Commons (flowRunner, liftFlow)
import Types.App (defaultGlobalState)
import Presto.Core.Types.Language.Flow (Flow)
import Types.App (GlobalState(..))

type Benefit = { icon :: String, label :: String, description :: String }
type FAQ = { question :: String, answer :: String }

screen :: ST.DriverClaimRewardScreenState -> LoggableScreen Action ST.DriverClaimRewardScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "DriverClaimRewardScreen"
  , globalEvents: [ \push -> do
                    when (isNothing initialState.data.bannerData.bannerItem) $ void $ launchAff $ flowRunner defaultGlobalState $ computeListItem push
                    pure $ pure unit
  ]
  , eval:
      ( \state action -> do
          let _ = spy "DriverClaimRewardScreenState -----" state
          let _ = spy "DriverClaimRewardScreenState--------action" action
          eval state action
      )
  , parent : Nothing
  , logWhitelist : []
  }

computeListItem :: (Action -> Effect Unit) -> Flow GlobalState Unit
computeListItem push = do
  bannerItem <- preComputeListItem $ Carousel.view push (Carousel.config BannerCarousal)
  liftFlow $ push (SetBannerItem bannerItem)

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
    , maybe (linearLayout [] []) (\item -> getCarouselView item push state) state.data.bannerData.bannerItem
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
        , text $ getStringV2 LT2.namma_kutumba
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
      , padding $ Padding 20 16 15 10
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
  , videoId: (\x -> getVideoID x) state.data.driverRewardConfig.youtubeVideoLink
  , videoType: "VIDEO"
  , videoHeight: 200
  , showFullScreen: false
  , hideFullScreenButton : false
  }

nominationView :: forall w. (Action -> Effect Unit) -> ST.DriverClaimRewardScreenState -> PrestoDOM (Effect Unit) w
nominationView push state =
  let nominationViewConfig = state.data.driverRewardConfig.nominationViewConfig
  in
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background "#F6E6FF" 
    , cornerRadius 12.0
    , visibility $ if nominationViewConfig.visibility then VISIBLE else GONE
    , margin $ Margin 16 16 16 0
    , onClick (\_ -> openUrlInApp $ nominationViewConfig.formLink) (const unit)
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        ]
        [ textView
            [ text $ getStringV2 LT2.nominate_your_representative
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
        , onClick (\_ -> openUrlInApp $ nominationViewConfig.videoLink) (const unit)
        ]
        [ textView
            [ textFromHtml $ "<u>" <> getStringV2 LT2.watch_video <> "</u>"
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
      nyMemberProbationTillDate = case state.data.nyMemberProbationTill of
        Just nyMemberProbationTill -> convertEpochToDateFormat $ show nyMemberProbationTill
        Nothing -> ""
      info =
        case driverTag of
          "ny_member_probation"   -> { label: getString (YOU_ARE_IN_PROBATION_PERIOD_TILL nyMemberProbationTillDate) <> "\n" <> getStringV2 LT2.pass_all_eligibility_criteria_to_continue_being_a_member, color: "#FFEBDD", visible: true, support: false }
          "ny_member_revoked"     -> { label: getStringV2 LT2.your_namma_kutumba_membership_has_been_revoked, color: "#FFE9E9", visible: true, support: true }
          _ -> { label: "", color: "#FFFFFF", visible: false, support: false }
  in
    infoCard info.label info.color info.visible info.support push

infoCardForNonEligibility :: forall w. (Action -> Effect Unit) -> ST.DriverClaimRewardScreenState -> PrestoDOM (Effect Unit) w
infoCardForNonEligibility push state =
  let info = { label: getStringV2 LT2.high_cancellation_rate_take_more_rides, color: "#FFF6E0", visible: (fromMaybe 0 state.data.cancellationRateInWindow) >= 30 , support: false }
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

benefits :: ST.DriverClaimRewardScreenState -> Array Benefit
benefits state =
  [ { icon: "ny_ic_medical_support"
    , label: getStringV2 LT2.accidental_medical_support
    , description: getStringV2 LT2.accidental_medical_support_description
    }
  , { icon: "ny_ic_death_support"
    , label: getStringV2 LT2.accidental_death_support
    , description: getStringV2 LT2.accidental_death_support_description
    }
  , { icon: "ny_ic_health_insurance"
    , label: getStringV2 LT2.medical_health_insurance
    , description: getStringV2 LT2.medical_health_insurance_description
    }
  , { icon: "ny_ic_personal_loans"
    , label: getStringV2 LT2.personal_loans
    , description: getStringV2 LT2.personal_loans_description
    }
  , { icon: "ny_ic_namma_support"
    , label: getStringV2 LT2.namma_support
    , description: getStringV2 LT2.namma_support_description
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
        , text $ getStringV2 LT2.benefits
        , color Color.black900
        , padding $ Padding 16 20 0 10
        ] <> FontStyle.h2 TypoGraphy
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ]
        (mapWithIndex (\i b -> benefitItem push i state b) (if state.props.showAllBenefits then benefits state else take 5 (benefits state)) <>
         if length (benefits state) > 5 && not state.props.showAllBenefits
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
  [{ label:getStringV2 LT2.completed_50_rides_in_the_last_3_months , visibility: (fromMaybe 0 state.data.numberOfRides) >= 50}
  , { label:getStringV2 LT2.safety_score_of_80_or_above , visibility: (fromMaybe 0 state.data.safetyScore) >= 80}
  , { label:getStringV2 LT2.rating_of_4_6_or_above , visibility: (fromMaybe 0.0 state.data.rating) >= 4.6}
  , { label:getStringV2 LT2.cancellation_rate_below_30 , visibility: (fromMaybe 0 state.data.cancellationRateInWindow) < 30}
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
        , text $ getStringV2 LT2.eligibility_criteria
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

faqs :: ST.DriverClaimRewardScreenState -> Array FAQ
faqs state =
  [ { question: getStringV2 LT2.what_is_kutumba
    , answer: getStringV2 LT2.what_is_kutumba_answer
    }
  , { question: getStringV2 LT2.how_can_i_be_eligible
    , answer: getStringV2 LT2.how_can_i_be_eligible_answer
    }
  , { question: getStringV2 LT2.can_i_refer_a_friend_to_kutumba_once_i_am_eligible
    , answer: getStringV2 LT2.can_i_refer_a_friend_to_kutumba_once_i_am_eligible_answer
    }
  , { question: getStringV2 LT2.how_do_i_claim_a_benefit
    , answer: getStringV2 LT2.how_do_i_claim_a_benefit_answer
    }
  , { question: getStringV2 LT2.are_there_limits_on_claims_per_month
    , answer: getStringV2 LT2.are_there_limits_on_claims_per_month_answer
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
            , if i == (length (faqs state) - 1) then
                linearLayout [] []
              else
                linearLayout
                  [ width MATCH_PARENT
                  , height $ V 1
                  , background "#E0E0E0"
                  ]
                  []
            ]
        ) (faqs state))
    ]

getCarouselView :: forall w. ListItem -> (Action -> Effect Unit) -> ST.DriverClaimRewardScreenState -> PrestoDOM (Effect Unit) w
getCarouselView view push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding $ Padding 16 16 16 16
    , visibility $ if state.data.driverRewardConfig.carouselVisibility then VISIBLE else GONE
    ] [ textView $
        [ text $ getString $ HOW_IT_WORKS ""
        , color Color.black900
        , margin $ MarginBottom 16
        ] <> FontStyle.h2 TypoGraphy
      , CarouselHolder.carouselView push $ getCarouselConfig view state]

getCarouselConfig ∷ forall a. ListItem → ST.DriverClaimRewardScreenState → CarouselHolder.CarouselHolderConfig Carousel.PropConfig Action
getCarouselConfig view state = {
    view
  , items : Carousel.bannerTransformer $ getVideoBannerConfigs state
  , orientation : VERTICAL
  , currentPage : state.data.bannerData.currentPage
  , autoScroll : true
  , autoScrollDelay : 5000.0
  , id : "bannerCarousel"
  , autoScrollAction : Just UpdateBanner
  , onPageSelected : Just BannerChanged
  , onPageScrollStateChanged : Just BannerStateChanged
  , onPageScrolled : Nothing
  , currentIndex : state.data.bannerData.currentBanner
  , showScrollIndicator : true
  , layoutHeight : V 100
  , overlayScrollIndicator : true
}

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
    , onClick (\_ -> openUrlInApp $ state.data.driverRewardConfig.termsAndConditionsLink) (const unit)
    ]
    [ textView
        [ text $ getStringV2 LT2.terms_and_conditions
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
  let claimButtonConfig = state.data.driverRewardConfig.claimButtonConfig
  in 
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
        , onClick push $ if claimButtonConfig.visibility then const OpenWhatsAppSupport else const NoAction
        ]
        [ textView
            [ text $ if claimButtonConfig.visibility then getStringV2 LT2.claim_via else getStringV2 LT2.coming_soon
            , color $ if claimButtonConfig.visibility then Color.yellow900 else Color.grey700
            , fontStyle $ FontStyle.semiBold LanguageStyle
            , textSize FontSize.a_18
            , gravity CENTER
            ]
        , imageView 
            [ width $ V 75
            , height $ V 18
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_whatsapp_green"
            , visibility $ if claimButtonConfig.visibility then VISIBLE else GONE
            , margin $ Margin 10 2 0 0
            ]
        ]
    ]

getBenefitsToShow :: ST.DriverClaimRewardScreenState -> Array Benefit
getBenefitsToShow state =
  if state.props.showAllBenefits
    then benefits state
    else take 5 (benefits state)

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
        , text if showingAll then getStringV2 LT2.view_less else getStringV2 LT2.view_more
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
