module Components.FavouriteDriverInfoCard.View where

import Prelude
import Components.FavouriteDriverInfoCard.Controller (Action(..), FavouriteDriverInfoCardState(..), donePrimaryButtonConfig)
import Effect (Effect)
import Prelude (Unit)
import PrestoDOM (FontWeight(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), Accessiblity(..), fontWeight, background, clickable, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, lineHeight, linearLayout, relativeLayout, frameLayout, margin, onClick, orientation, padding, text, textSize, textView, visibility, weight, width, textFromHtml, onBackPressed, scrollView, imageWithFallback, stroke, afterRender, singleLine, ellipsize, accessibility, accessibilityHint)
import Styles.Colors as Color
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Engineering.Helpers.Commons (screenWidth)
import PrestoDOM.Properties (lineHeight, cornerRadii)
import Data.Array ((:))
import PrestoDOM.Types.DomAttributes (Gravity(..), Corners(..))
import Font.Size as FontSize
import Font.Style as FontStyle
import Common.Types.App (LazyCheck(..))
import Components.PrimaryButton (view) as PrimaryButton
import Language.Strings (getString)
import Language.Types (STR(..))
import Engineering.Helpers.Commons (os)

view :: forall w . (Action  -> Effect Unit) -> FavouriteDriverInfoCardState -> PrestoDOM (Effect Unit) w
view push config =
    relativeLayout[
        height MATCH_PARENT,
        width  MATCH_PARENT,
        background Color.white900
    ][
        scrollView[
            height MATCH_PARENT,
            width  MATCH_PARENT
        ][
            linearLayout[
                height MATCH_PARENT,
                width MATCH_PARENT,
                orientation VERTICAL
            ][
                linearLayout[
                    height WRAP_CONTENT,
                    width MATCH_PARENT,
                    orientation VERTICAL,
                    padding $ Padding 16 16 16 32,
                    background Color.blue600
                ][
                    imageView
                        [ width $ V 25
                        , height $ V 25
                        , margin $ MarginBottom 17
                        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
                        , onClick push $ const Back
                        ]
                ,   imageView
                        [ width MATCH_PARENT 
                        , height $ V ((267 * ((screenWidth unit)-32))/328)
                        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_favourite_driver_feat" 
                        ]
                ]
            ,   linearLayout[
                    height WRAP_CONTENT,
                    width MATCH_PARENT,
                    orientation VERTICAL,
                    cornerRadii (Corners 24.0 true true false false),
                    background $ Color.white900,
                    padding $ PaddingHorizontal 16 16
                ][
                    textView (
                        [ text $ getString FAVOURITE_YOUR_DRIVER
                        , color Color.black800
                        , accessibility ENABLE
                        , fontWeight $ FontWeight 700
                        , textSize FontSize.a_18
                        , margin $ MarginTop 32
                        , lineHeight "22"
                        , singleLine false
                        ])
                ,   linearLayout[
                        height WRAP_CONTENT,
                        width MATCH_PARENT,
                        margin $ MarginTop 20,
                        gravity CENTER
                    ][
                        imageView
                            [ width $ V 13
                            , height $ V 16
                            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_green_stars"
                            , gravity CENTER
                            , margin $ MarginRight 10
                            ]
                    ,   textView (
                        [ text $ getString FAVOURITE_DRIVER_PREFERENCE
                        , color Color.black800
                        , accessibility ENABLE
                        , singleLine false
                        ] <> FontStyle.paragraphText TypoGraphy)
                    ]
                ,   linearLayout[
                        height WRAP_CONTENT,
                        width MATCH_PARENT,
                        margin $ MarginTop 20,
                        gravity CENTER
                    ][
                        imageView
                            [ width $ V 13
                            , height $ V 16
                            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_green_stars"
                            , gravity CENTER
                            , margin $ MarginRight 10
                            ]
                    ,   textView (
                        [ text $ getString RIDE_TYPE_WITH_FAVOURITE_DRIVER
                        , color Color.black800
                        , accessibility ENABLE
                        , singleLine false
                        ] <> FontStyle.paragraphText TypoGraphy)
                    ]
                ]
            ]
        ]
    ,   linearLayout[
            height MATCH_PARENT,
            width  MATCH_PARENT,
            orientation VERTICAL
        ][
            linearLayout[weight 1.0][],
            linearLayout[
                height WRAP_CONTENT,
                width  MATCH_PARENT
            ][
                PrimaryButton.view (push <<< OnClickDone) (donePrimaryButtonConfig)
            ]
        ]
    ]