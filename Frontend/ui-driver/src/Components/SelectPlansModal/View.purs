module Components.SelectPlansModal.View where

import Prelude
import Data.Maybe
import Effect (Effect)
import Data.Array as DA
import Data.String as DS
import Screens.Types as ST
import Helpers.Utils as HU
import Styles.Colors as Color
import Mobility.Prelude as MP
import Font.Style as FontStyle
import Language.Types (STR(..))
import Components.PlanCard as PlanCard
import Common.Types.App (LazyCheck(..))
import PrestoDOM.Properties (cornerRadii)
import Components.PrimaryButton as PrimaryButton
import Language.Strings (getString, getVarString)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Components.SelectPlansModal.Controller (Action(..), SelectPlansState(..))
import PrestoDOM (alignParentRight, alignParentLeft, Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), afterRender, alignParentBottom, alpha, background, clickable, color, cornerRadius, ellipsize, fontStyle, frameLayout, gradient, gravity, height, horizontalScrollView, id, imageView, imageWithFallback, lineHeight, linearLayout, lottieAnimationView, margin, maxLines, onBackPressed, onClick, orientation, padding, relativeLayout, scrollBarX, scrollBarY, scrollView, shimmerFrameLayout, singleLine, stroke, text, textFromHtml, textSize, textView, visibility, weight, width)

view :: forall w. (Action -> Effect Unit) -> SelectPlansState -> PrestoDOM (Effect Unit) w
view push state = 
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , background Color.black9000
    , gravity CENTER
    , clickable true
    ][  linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , gravity CENTER
        , background Color.white900
        , margin $ Margin 24 16 24 16
        , padding $ Padding 16 16 16 16
        , cornerRadius 16.0
        ][  imageView
            [ width $ V 40
            , height $ V 40
            , imageWithFallback $ HU.fetchImage HU.COMMON_ASSET "ny_ic_completed_v2"
            ],
            textView $
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER
            , color Color.black800
            , margin $ Margin 8 0 8 0
            , text $ getString CHOOSE_A_PLAN
            ] <> FontStyle.h2 TypoGraphy,
            textView $
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER
            , color Color.black700
            , margin $ Margin 8 0 8 0
            , text $ getString YOU_HAVE_SWITCHED_CITY_OR_VEHICLE
            ] <> FontStyle.body1 TypoGraphy,
            plansListView push state,
            contactSupportView push state,
            buttonsView push state
        ]
    ]

plansListView :: forall w. (Action -> Effect Unit) -> SelectPlansState -> PrestoDOM (Effect Unit) w
plansListView push state = 
    linearLayout 
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 18
    ] (map (\item -> PlanCard.view (push <<< (PlanCardAction item)) item) state.plansList)

contactSupportView :: forall w. (Action -> Effect Unit) -> SelectPlansState -> PrestoDOM (Effect Unit) w
contactSupportView push state = 
    linearLayout 
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER
    , margin $ MarginTop 18
    , onClick push $ const Support
    ] [ imageView
        [ width $ V 24
        , height $ V 24
        , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_phone_filled_blue"
        ],
        textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER
        , margin $ MarginLeft 10
        , padding $ PaddingBottom 5
        , color Color.black800
        , textFromHtml $ "<span style='color:#2194FF'> <u>"<> getString CONTACT_SUPPORT_FOR_HELP <>"<u/> </span>"
        ] <> FontStyle.subHeading2 TypoGraphy
    ]

buttonsView :: forall w. (Action -> Effect Unit) -> SelectPlansState -> PrestoDOM (Effect Unit) w
buttonsView push state = 
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 24
    ][ PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state),
        textView $
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , color Color.black650
        , padding $ Padding 16 16 16 16
        , text $ getString DISMISS
        , onClick push $ const Dismiss
        ] <> FontStyle.subHeading2 TypoGraphy
    ]
    where 
        primaryButtonConfig :: SelectPlansState -> PrimaryButton.Config 
        primaryButtonConfig state =
            PrimaryButton.config
                { textConfig
                { text = case state.selectedPlan of 
                            Just plan -> getString $ CONTINUE_WITH plan.title
                            Nothing -> getString CONTINUE
                , color = Color.primaryButtonColor
                }
                , margin = MarginHorizontal 16 16
                , background = Color.black900
                , height = V 54
                , width = MATCH_PARENT
                , id = "SelectPlansModalPrimaryButton"
                , isClickable = isJust state.selectedPlan
                , alpha = if isJust state.selectedPlan then 1.0 else 0.5
                }