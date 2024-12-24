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
import Presto.Core.Types.Language.Flow (Flow, doAff, delay)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..) ,background, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, relativeLayout, scrollView, shimmerFrameLayout, onBackPressed, alignParentBottom, singleLine, accessibilityHint,accessibility,accessibilityHint, Accessiblity(..), id, afterRender, layoutGravity, rippleColor, maxLines, ellipsize, onAnimationEnd, scrollBarY, fillViewport)
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
    relativeLayout
        [
            height MATCH_PARENT
        ,   width MATCH_PARENT
        ,   orientation VERTICAL
        ,   onBackPressed push $ const GoBack
        ,   padding $ PaddingVertical EHC.safeMarginTop EHC.safeMarginBottom
        ,   onClick push $ const NoAction
        ]
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
        ]