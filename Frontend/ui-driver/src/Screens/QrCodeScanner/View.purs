module Screens.QrCodeScanner.View where

import Prelude

import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Animation (fadeIn)
import Effect.Uncurried (runEffectFn2)
import Types.App (defaultGlobalState)
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Control.Monad.Trans.Class (lift)
import Engineering.Helpers.Commons as EHC
import Effect (Effect)
import Screens.Types as ST
import Helpers.Utils as HU
import Animation as Anim
import Resource.Localizable.StringsV2 as StringsV2
import Resource.Localizable.TypesV2 as LT2
import Presto.Core.Types.Language.Flow (Flow, doAff, delay)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..) ,background, color, cornerRadius, gravity, height, frameLayout, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, relativeLayout, scrollView, shimmerFrameLayout, onBackPressed, alignParentBottom, singleLine, accessibilityHint,accessibility,accessibilityHint, Accessiblity(..), id, afterRender, layoutGravity, rippleColor, maxLines, ellipsize, onAnimationEnd, scrollBarY, fillViewport)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Keyed as Keyed
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.QrCodeScanner.Controller 

qrCodeScanner :: ST.QrCodeScannerState -> Screen Action ST.QrCodeScannerState ScreenOutput
qrCodeScanner initialState =
    {
        initialState
    ,   view
    ,   name: "QrCodeScanner"
    ,   globalEvents: [
        (\push -> pure $ pure unit)
    ]
    ,   eval:
        \action state -> eval action state
    }

view :: forall w. (Action -> Effect Unit) -> ST.QrCodeScannerState -> PrestoDOM (Effect Unit) w
view push state =
    Anim.screenAnimation $
    frameLayout 
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , onBackPressed push $ const GoBack
    ] $ 
    [
        linearLayout
        [
            height MATCH_PARENT
        ,   width MATCH_PARENT
        ,   afterRender (\action -> do
                void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ lift $ lift $ doAff do
                    liftEffect $ HU.startQRScanner push action (EHC.getNewIDWithTag "QrCodeScanner") 
                ) (const $ StartQRScanner)
        ,   id $ EHC.getNewIDWithTag "QrCodeScanner"
        ][]
    ,   relativeLayout
        [ height $ V 56
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        , padding $ PaddingHorizontal 16 16
        , margin $ MarginTop 32
        , onClick push $ const GoBack
        ]
        [
            linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT 
            ][
                imageView
                [ 
                height $ V 24
                , width $ V 24
                , margin $ MarginRight 16
                , imageWithFallback $ HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_chevron_left_white"
                ]
                ,
                textView
                [ text $ StringsV2.getStringV2 LT2.scan_bus_qr
                , textSize 18
                , color "#FFFFFF" 
                ]
            ]
        ]
    ,   relativeLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , gravity CENTER
        ]
        [
            imageView
            [ height $ WRAP_CONTENT
            , width $ WRAP_CONTENT
            , padding $ Padding 30 30 30 30
            , imageWithFallback $ HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_qr_frame" 
            , accessibilityHint "SCAN QR"
            ]
        ]
    ]