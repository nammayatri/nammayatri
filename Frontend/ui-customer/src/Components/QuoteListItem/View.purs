{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.QuoteListItem.View where

import Common.Types.App

import Animation (translateInXForwardAnim)
import Common.Types.App (LazyCheck(..))
import Components.QuoteListItem.Controller (Action(..))
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (ceil)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (flowRunner, os, parseFloat, screenWidth)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), getDefaultPixelSize)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, discard, pure, show, unit, ($), (/=), (<>), (==), (-), (*), (/))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, alpha, background, clickable, color, cornerRadius, disableClickFeedback, fontStyle, frameLayout, gradient, gravity, height, imageUrl, imageView, imageWithFallback, lineHeight, linearLayout, margin, onClick, orientation, padding, stroke, text, textSize, textView, visibility, weight, width, ellipsize, maxLines, singleLine, horizontalScrollView, scrollBarX)
import PrestoDOM.Animation as PrestoAnim
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color
import Types.App (defaultGlobalState)
import Timers
import Debug
import Engineering.Helpers.Commons (liftFlow)
import Screens.Types (QuoteListItemState(..), City(..))
import Locale.Utils
import JBridge(getWidthFromPercent)
import Data.String
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Mobility.Prelude(boolToVisibility)

view :: forall w . (Action  -> Effect Unit) -> QuoteListItemState -> PrestoDOM (Effect Unit) w
view push state =
  let isActive = state.selectedQuote == Just state.id
      padding' = Padding 16 (if isActive then (if os == "IOS" then 24 else 20) else 16) 16 (if isActive then 24 else 16)
  in  
  PrestoAnim.animationSet
  [ translateInXForwardAnim (state.timer /= "0") , translateInXForwardAnim (state.timer == "0")]  $
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , cornerRadius 6.0
    , margin (Margin 16 20 16 4)
    , background if state.selectedQuote == Just state.id then Color.blue600' else Color.white900
    , stroke if state.selectedQuote == Just state.id then ("1,"<>Color.blue700') else ("1," <> Color.grey)
    , afterRender (\action -> do
                    _ <- push action
                    _ <- launchAff $ flowRunner defaultGlobalState $ liftFlow $ startTimer state.seconds state.id "1" push CountDown
                    pure unit
                  ) (const NoAction)
    , onClick push (const $ Click state)
    , disableClickFeedback true
    , padding padding'
    ][ linearLayout[
      orientation HORIZONTAL
    , width MATCH_PARENT
    , height WRAP_CONTENT
    ][ driverImageView state
     , linearLayout
       [ height WRAP_CONTENT
       , width MATCH_PARENT
       , padding $ PaddingLeft 12
       , orientation VERTICAL
       ][ serviceAndPrice state push
        , horizontalLine state
        , ratingAndDistanceView state push
      ]
    ]
   , primaryButtonView state push
  ]

ratingAndDistanceView :: forall w . QuoteListItemState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
ratingAndDistanceView state push =
 let layoutWidth = (screenWidth unit) - 140
     scrollViewWidth = layoutWidth / 2
 in
 linearLayout
 [ width $ V $ layoutWidth
 , height WRAP_CONTENT
 , gravity CENTER_VERTICAL
 ][ linearLayout 
    [ height WRAP_CONTENT
    , width $ if os == "IOS" then V $ scrollViewWidth else WRAP_CONTENT 
    ][driverRatingView state
     , distanceView state push 
    ]
  , linearLayout
    [ height WRAP_CONTENT
    , width $ if os == "IOS" then V $ (scrollViewWidth - 4) else MATCH_PARENT
    , gravity RIGHT
    , margin $ MarginLeft 4 
    ][ textView $ 
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , singleLine false
      , text $ case (getLanguageLocale languageKey) of
            "EN_US" -> (getString EXPIRES_IN) <>" : " <> state.timer <> "s"
            "FR_FR" -> (getString EXPIRES_IN) <>" : " <> state.timer <> "s"
            _ -> state.timer <> "s " <> (getString EXPIRES_IN)
      , color state.appConfig.quoteListItemConfig.expiresColor
      , gravity CENTER_VERTICAL
      ] <> FontStyle.body3 LanguageStyle
   ] 
 ]

driverImageView :: forall w . QuoteListItemState -> PrestoDOM (Effect Unit) w
driverImageView state =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][ frameLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity LEFT
    , margin $ MarginTop if os == "IOS" then 0 else 4
    ][imageView
      [ width $ V 36
      , height $ V 36
      , background state.appConfig.quoteListItemConfig.driverImagebg
      , cornerRadius 18.0
      , margin $ MarginLeft 25
      , imageWithFallback state.profile
      ]
    , imageView
      [ height $ V state.appConfig.quoteListItemConfig.vehicleHeight
      , width $ V state.appConfig.quoteListItemConfig.vehicleWidth
      , cornerRadius 20.0
      , imageWithFallback $ fetchImage FF_ASSET $ state.vehicleImage
      ]
    ]
  ]

serviceAndPrice :: forall w . QuoteListItemState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
serviceAndPrice state push =
  let layoutWidth = (screenWidth unit) - 140
  in 
  linearLayout
  [ height MATCH_PARENT
  , width $ V layoutWidth 
  , gravity TOP_VERTICAL
  ][linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    , gravity TOP_VERTICAL
    ][textView $
      [ height MATCH_PARENT
      , width $ V $ layoutWidth * 13/20
      , text $ fromMaybe "" state.serviceTierName
      , color Color.black800
      , singleLine false
      , gravity TOP_VERTICAL
      ] <> FontStyle.subHeading3 LanguageStyle
    , textView $ 
      [ height MATCH_PARENT
      , width $ V $ layoutWidth * 13/20
      , gravity TOP_VERTICAL
      , singleLine false
      , visibility $ boolToVisibility $ isJust state.vehicleModel
      , text $ fromMaybe "" state.vehicleModel
      , color Color.black700
      ] <> FontStyle.tags LanguageStyle
    ]
  , linearLayout
    [ height WRAP_CONTENT
    , width $ if os == "IOS" then V $ layoutWidth * 7/20 else MATCH_PARENT
    , gravity RIGHT
    ][ textView $
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , gravity RIGHT
     , singleLine false
     , text $ state.appConfig.currency <> " " <> state.price
     , color Color.black800
     ] <> FontStyle.body8 TypoGraphy
    ]
  ]

driverRatingView :: forall w . QuoteListItemState -> PrestoDOM (Effect Unit) w
driverRatingView state  =
  let isActive = state.selectedQuote == Just state.id
  in
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  , cornerRadius 4.0
  , margin $ MarginRight 4
  , background if isActive then Color.white900 else Color.blue600
  , padding $ Padding 6 4 6 4
  ][imageView
    [ height $ V 10
    , width $ V 10
    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_star_active"
    , margin $ MarginRight 4
    ]
  , textView $ 
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , color $ Color.black700
    , text $ if state.driverRating == 0.0 then (getString NEW_) else parseFloat state.driverRating 2
    ] <> FontStyle.body16 LanguageStyle
  ]

distanceView :: forall w . QuoteListItemState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
distanceView state push =
  let isActive = state.selectedQuote == Just state.id
  in
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  , cornerRadius 4.0
  , background if isActive then Color.white900 else Color.blue600 
  , padding $ Padding 6 4 6 4
  ][ textView $ 
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , color Color.black700
    , text $ state.distanceToPickup
    ] <> FontStyle.body16 LanguageStyle
  ] 

primaryButtonView :: QuoteListItemState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
primaryButtonView state push =
 linearLayout
  ([ height WRAP_CONTENT
  , width MATCH_PARENT
  , padding $ PaddingVertical 14 14
  , margin $ MarginTop 24
  , visibility if state.selectedQuote == Just state.id then VISIBLE else GONE
  , cornerRadius state.appConfig.quoteListItemConfig.primaryButtonCorner
  , onClick push $ const ConfirmRide
  , gravity CENTER
  ] <> if state.appConfig.primaryButtonConfig.isGradient then [gradient (Linear 90.0 state.appConfig.primaryButtonConfig.gradient)] else [background state.appConfig.primaryBackground])
  [ textView (
     [ width WRAP_CONTENT
     , height WRAP_CONTENT
     , text (getString CONFIRM_RIDE_)
     , color state.appConfig.primaryTextColor
     ] <> FontStyle.subHeading1 LanguageStyle)
  ]

autoAcceptingView :: forall w . QuoteListItemState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
autoAcceptingView state push =
 linearLayout
 [ height WRAP_CONTENT
 , width MATCH_PARENT
 , gravity RIGHT
 , weight 1.0
 , padding $ PaddingLeft 10
 ][ frameLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    ][ --PrestoAnim.animationSet [translateOutXForwardAnim ((getValueToLocalStore AUTO_SELECTING) == state.id)] $
       linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , background "#ABC8F4"
        , alpha 0.3
        , color Color.black900
        , clickable false
        , padding (Padding 16 5 16 5)
        ][ linearLayout
            [ width MATCH_PARENT
            , height $ V 18
            ][]
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding (Padding 16 5 16 5)
        , stroke ("1,#0066FF")
        , cornerRadius 5.0
        , clickable true
        ][ textView (
            [ height MATCH_PARENT
            , weight 1.0
            , gravity CENTER_VERTICAL
            , color Color.black900
            , text $ case (getLanguageLocale languageKey) of
                "EN_US" -> (getString AUTO_ACCEPTING_SELECTED_RIDE) <> " : " <> state.timer <> "s"
                _ -> state.timer <> "s " <> (getString AUTO_ACCEPTING_SELECTED_RIDE)
            ] <> FontStyle.tags LanguageStyle)
          , imageView
            [ height $ V 18
            , width $ V 18
            , imageWithFallback $ fetchImage FF_COMMON_ASSET  "ny_ic_close"
            , onClick push $ const CancelAutoAssigning
            ]
        ]

    ]
 ]


horizontalLine :: forall w . QuoteListItemState -> PrestoDOM (Effect Unit) w
horizontalLine state =
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , margin $ MarginVertical 12 12
 , gravity CENTER
 ][ linearLayout
    [ width MATCH_PARENT
    , height $ V 1
    , background Color.grey
    ][]
 ]
