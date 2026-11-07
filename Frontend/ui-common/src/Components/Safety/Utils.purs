module Components.Safety.Utils where

import PrestoDOM
import Data.Maybe as Mb
import Common.Types.App (LazyCheck(..), RateCardType(..))
import Data.String as DS
import Data.Int as DI
import Data.Maybe as DM
import Animation (translateInXForwardAnim, translateInXBackwardAnim)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, ($), const, (<>), (>),(==), (||), (&&), (/), (*), (/=), (+), (<<<), unit, map, (-), not)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, color, cornerRadius, imageUrl, fontStyle, gravity, height, imageView, textFromHtml,imageWithFallback, linearLayout, margin, onClick, orientation, padding, text, textSize, textView, visibility, weight, width, lineHeight,fontStyle, scrollView, maxLines, singleLine, stroke, horizontalScrollView, relativeLayout)
import PrestoDOM.Properties (cornerRadii)
import Styles.Colors as Color
import PrestoDOM.Animation as PrestoAnim
import Animation.Config as AnimConfig
import Halogen.VDom.DOM.Prop (Prop)
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Components.PrimaryButton as PrimaryButton
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Mobility.Prelude (boolToVisibility)

type MeasureViewConfig
  = { text' :: String
    , isActive :: Boolean
    , textColor :: String
    , useMargin :: Boolean
    , usePadding :: Boolean
    , useFullWidth :: Boolean
    , image :: Mb.Maybe String
    , visibility :: Boolean
    , textStyle :: FontStyle.Style
    , showBullet :: Boolean
    }

measureView :: MeasureViewConfig -> forall w. PrestoDOM (Effect Unit) w
measureView config =
  linearLayout
    ( [ height WRAP_CONTENT
      , width if config.useFullWidth then MATCH_PARENT else WRAP_CONTENT
      , visibility $ boolToVisibility config.visibility
      , alpha $ if config.isActive then 1.0 else 0.6
      , accessibility ENABLE
      , accessibilityHint $ config.text' <> " : Button"
      ]
        <> if config.useMargin then
            [ margin $ MarginTop 12 ]
          else
            []
              <> if config.usePadding then [ padding $ PaddingHorizontal 16 16 ] else []
    )
    [ imageView
        [ imageWithFallback
            $ fetchImage FF_ASSET case config.image of
                Just image' -> image'
                Nothing -> "ny_ic_check"
        , height $ V 20
        , width $ V 20
        , margin $ MarginRight 8
        , visibility $ boolToVisibility $ not config.showBullet
        ]
    , textView
        $ [ text "•"
          , color config.textColor
          , height WRAP_CONTENT
          , visibility $ boolToVisibility config.showBullet
          , margin $ MarginRight 4
          ]
        <> (FontStyle.getFontStyle config.textStyle LanguageStyle)
    , textView
        $ [ text config.text'
          , color config.textColor
          , weight 1.0
          , height WRAP_CONTENT
          , singleLine false
          ]
        <> (FontStyle.getFontStyle config.textStyle LanguageStyle)
    ]

layoutWithWeight :: forall w. PrestoDOM (Effect Unit) w
layoutWithWeight = linearLayout [ weight 1.0 ] []