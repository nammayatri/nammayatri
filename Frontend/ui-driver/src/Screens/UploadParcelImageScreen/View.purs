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
import PrestoDOM (BottomSheetState(..), Gravity(..), InputType(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, LoggableScreen, Visibility(..), afterRender, alignParentBottom, alignParentRight, alpha, background, clickable, color, cornerRadius, editText, ellipsize, fontStyle, frameLayout, gravity, height, hint, id, imageUrl, imageView, imageWithFallback, inputType, inputTypeI, layoutGravity, linearLayout, margin, maxLines, onBackPressed, onChange, onClick, orientation, padding, pattern, relativeLayout, scrollView, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, scrollBarY)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Properties as PP
import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM.Types.DomAttributes as PTD
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Styles.Colors as Color
import Components.GenericHeader.Controller as GenericHeaderConfig
import Components.GenericHeader.View as GenericHeader
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


screen :: ST.UploadParcelImageScreenState -> LoggableScreen Action ST.UploadParcelImageScreenState ScreenOutput
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
  , parent : Nothing
  , logWhitelist: initialState.data.config.logWhitelistConfig.uploadParcelImageScreenLogWhitelist
  }


view :: forall w. (Action -> Effect Unit) -> ST.UploadParcelImageScreenState -> PrestoDOM (Effect Unit) w
view push state =
    Anim.screenAnimation $
    relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background $ if state.props.showConfirmAndUploadButton then Color.black else Color.white900
        , onBackPressed push $ const BackPressed
        , padding $ PaddingVertical EHC.safeMarginTop EHC.safeMarginBottom
        , onClick push $ const NoAction
        ]
        [
        PrestoAnim.animationSet
        [ Anim.fadeIn true] $
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        [ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
        , separatorView push state 
        ]
        , uploadInstructionsView push state
        , confirmAndUploadView push state
        , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , gravity BOTTOM
        , margin (MarginBottom 24)
        , alignParentBottom "true,-1"
        , stroke $ if state.props.showConfirmAndUploadButton then "0," <> Color.black else ("1,"<> Color.grey900)
        , cornerRadii $ if state.props.showConfirmAndUploadButton then (Corners 16.0 false false false false) else (Corners 16.0 true true false false)
        ]
        [ PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig state)
        ]
    ]

headerView :: forall w. (Action -> Effect Unit) -> ST.UploadParcelImageScreenState -> PrestoDOM (Effect Unit) w
headerView push state = GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)


uploadInstructionsView :: forall w. (Action -> Effect Unit) -> ST.UploadParcelImageScreenState -> PrestoDOM (Effect Unit) w
uploadInstructionsView push state =
    scrollView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ Margin 20 70 20 80
    , visibility $ boolToVisibility $ not state.props.showConfirmAndUploadButton
    ][linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ][ howToUpload push state
    ]]
    
howToUpload :: forall w. (Action -> Effect Unit) -> ST.UploadParcelImageScreenState -> PrestoDOM (Effect Unit) w
howToUpload push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin (MarginTop 20) 
  ][ textView $ 
    [ text $ getString HOW_TO_UPLOAD
    , color Color.greyTextColor
    ] <> FontStyle.h3 TypoGraphy
  , linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginVertical 0 10
    , padding $ Padding 0 16 0 16
    ][ 
      textView $ 
      [ text $ getString TAKE_CLEAR_PICTURE_PARCEL
      , color Color.black800
      , margin $ MarginBottom 18
      ] <> FontStyle.body3 TypoGraphy
    , textView $ 
      [ text $ getString ENSURE_ADEQUATE_LIGHT_PARCEL_DESC
      , color Color.black800
      , margin $ MarginBottom 18
      ] <> FontStyle.body3 TypoGraphy
    , textView $ 
      [ text $ getString FIT_PARCEL_CORRECTLY
      , color Color.black800
      , margin $ MarginBottom 40
      ] <> FontStyle.body3 TypoGraphy
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , cornerRadius 4.0
      , margin $ MarginTop 20
      , stroke $ "1," <> Color.borderGreyColor
      , padding $ Padding 16 16 16 0
      ]( map rightWrongView (uploadParcelInstructionData unit))
    ]
  ]


rightWrongView :: {image :: String, icon :: String, instructions :: Array String} -> forall w . PrestoDOM (Effect Unit) w
rightWrongView item = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER_VERTICAL
  , margin $ MarginBottom 16
  ][ imageView
    [ width $ V 120
    , height $ V 100
    , imageWithFallback $ fetchImage FF_ASSET $ item.image
    ]
  , linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , padding $ Padding 16 16 0 0
    , gravity CENTER
    ](map (rightWrongItemView item.icon) item.instructions)
  ]

rightWrongItemView :: String -> String -> forall w . PrestoDOM (Effect Unit) w
rightWrongItemView image text' = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ MarginBottom 5
  , gravity CENTER_VERTICAL
  ][ imageView
    [ width $ V 16
    , height $ V 16
    , imageWithFallback $ fetchImage FF_ASSET $ image
    ]
  , textView $
    [ text text'
    , color Color.black800
    , margin $ MarginLeft 8
    ] <> FontStyle.body1 TypoGraphy
  ]

confirmAndUploadView :: forall w. (Action -> Effect Unit) -> ST.UploadParcelImageScreenState -> PrestoDOM (Effect Unit) w
confirmAndUploadView push state =
    scrollView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ Margin 16 70 16 80
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
    ]
    [   linearLayout
        [ width MATCH_PARENT
        , height (V 400)
        , orientation VERTICAL
        , layoutGravity "center"
        , id (EHC.getNewIDWithTag "confirmImageView")
        ]
        [],
        textView $
        [ 
        width WRAP_CONTENT
        , text (getString SENDER_WILL_VERIFY_PARCEL)
        , color Color.white900
        , margin (Margin 0 24 0 24)
        , gravity CENTER
        ] <> FontStyle.body1 TypoGraphy
    ]
    ]

separatorView :: forall w. (Action -> Effect Unit) -> ST.UploadParcelImageScreenState -> PrestoDOM (Effect Unit) w
separatorView push state =
  linearLayout
  [ width MATCH_PARENT
  , height $ V 1
  , margin $ MarginVertical 5 5
  , background Color.grey900
  ]
  []