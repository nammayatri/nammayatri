module Screens.UploadParcelImageScreen.View where

import Prelude
import Common.Types.App
import Data.Maybe
import Animation as Anim
import Data.String as DS
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn1)
import Common.Types.App (LazyCheck(..))
import PrestoDOM (BottomSheetState(..), Gravity(..), InputType(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alignParentBottom, alignParentRight, alpha, background, clickable, color, cornerRadius, editText, ellipsize, fontStyle, frameLayout, gravity, height, hint, id, imageUrl, imageView, imageWithFallback, inputType, inputTypeI, layoutGravity, linearLayout, margin, maxLines, onBackPressed, onChange, onClick, orientation, padding, pattern, relativeLayout, scrollView, stroke, text, textFromHtml, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Properties as PP
import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM.Types.DomAttributes as PTD
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Styles.Colors as Color
import Language.Strings (getString)
import Engineering.Helpers.Commons as EHC
import Language.Types (STR(..))
import Types.App (GlobalState(..), defaultGlobalState)
import Prelude (Unit, const, ($), (<<<), (<>), bind, discard, unit, pure, map, (==), (/=))
import Mobility.Prelude
import Screens.UploadParcelImageScreen.Controller
import Components.PrimaryButton as PrimaryButton
import Screens.Types as ST
import JBridge as JB
import Debug (spy)
import Font.Style as FontStyle
import Screens.UploadParcelImageScreen.ComponentConfig
import Effect.Uncurried (runEffectFn2)


screen :: ST.UploadParcelImageScreenState -> Screen Action ST.UploadParcelImageScreenState ScreenOutput
screen initialState = 
    {
        initialState
    ,   view
    ,   name : "UploadParcelImageScreen"
    ,   globalEvents : [(\push -> do
        void $ JB.storeCallBackImageUpload push CallBackImageUpload
        void $ runEffectFn2 JB.storeCallBackUploadMultiPartData push UploadMultiPartDataCallback
        pure $ pure unit 
        )]
    ,   eval: 
        ( \state action -> do
            let _ = spy "UploadParcelImage state -----" state
            let _ = spy "UploadParcelImage--------action" action
            eval state action)
    }

view :: forall w. (Action -> Effect Unit) -> ST.UploadParcelImageScreenState -> PrestoDOM (Effect Unit) w
view push state =
    Anim.screenAnimation $
    frameLayout
    [
        height MATCH_PARENT
    ,   width MATCH_PARENT
    ][
        linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push $ const BackPressed
        , afterRender push (const AfterRender)
        , background Color.black
        ][
            headerLayout push state
        ,   captureParcelImageView push state
        ,   confirmAndUploadButton push state
        ]
    ]

headerLayout :: forall w. (Action -> Effect Unit) -> ST.UploadParcelImageScreenState -> PrestoDOM (Effect Unit) w
headerLayout push state =
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation HORIZONTAL
        , layoutGravity "center_vertical"
        , padding $ Padding 16 16 16 16
        , gravity CENTER_VERTICAL
        ]
        [ imageView
            [ width $ V 24
            , height $ V 24
            , imageWithFallback $ fetchImage FF_ASSET $ "ny_ic_chevron_left_white"
            , gravity CENTER_VERTICAL
            , onClick push $ const BackPressed
            ]
        , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ getString TAKE_PHOTO_OF_PARCEL
              , weight 1.0
              , gravity CENTER_VERTICAL
              , color Color.white900
              , padding $ Padding 16 0 0 0
              ]
            <> FontStyle.h3 TypoGraphy
        ]
    ]

captureParcelImageView :: forall w. (Action -> Effect Unit) -> ST.UploadParcelImageScreenState -> PrestoDOM (Effect Unit) w
captureParcelImageView push state =
    scrollView
    [ width MATCH_PARENT
    , height $ V (EHC.screenHeight unit - 100) 
    , visibility $ boolToVisibility $ not state.props.showConfirmAndUploadButton
    ][
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , layoutGravity "center"
    , id (EHC.getNewIDWithTag "captureParcelImageView")
    , visibility $ boolToVisibility $ not state.props.showConfirmAndUploadButton
    ]
    [ 
    ]
    ]

confirmAndUploadButton :: forall w. (Action -> Effect Unit) -> ST.UploadParcelImageScreenState -> PrestoDOM (Effect Unit) w
confirmAndUploadButton push state =
    scrollView
    [ width MATCH_PARENT
    , height $ V (EHC.screenHeight unit - 100) 
    , visibility $ boolToInvisibility $ state.props.showConfirmAndUploadButton
    ][
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , layoutGravity "center"
    , gravity CENTER
    , margin (Margin 0 50 0 50)
    , weight 1.0
    , visibility $ boolToInvisibility $ state.props.showConfirmAndUploadButton
    ]
    [   linearLayout
        [ width MATCH_PARENT
        , height (V 400)
        , orientation VERTICAL
        , layoutGravity "center"
        , id (EHC.getNewIDWithTag "confirmImageView")
        , visibility $ boolToInvisibility $ state.props.showConfirmAndUploadButton
        ]
        [],
        textView $
        [ 
          width WRAP_CONTENT
        , text (getString SENDER_WILL_VERIFY_PARCEL)
        , color Color.white900
        , margin (Margin 0 24 0 24)
        , gravity CENTER
        , visibility $ boolToVisibility $ state.props.isStartRideActive
        ] <> FontStyle.body1 TypoGraphy
    ,   PrimaryButton.view (push <<< ConfirmAndUpload) (confirmAndUploadButtonConfig state)
    ]
    ]