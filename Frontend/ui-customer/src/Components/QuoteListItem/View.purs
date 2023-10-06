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
import Components.QuoteListItem.Controller (Action(..), QuoteListItemState)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Maybe (Maybe(..))
import Data.Number (ceil)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (flowRunner, os, countDown)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import JBridge (startTimerWithTime)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, discard, pure, show, unit, ($), (/=), (<>), (==))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, alpha, background, clickable, color, cornerRadius, disableClickFeedback, fontStyle, frameLayout, gradient, gravity, height, imageUrl, imageView, imageWithFallback, lineHeight, linearLayout, margin, onClick, orientation, padding, stroke, text, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color
import Types.App (defaultGlobalState)

view :: forall w . (Action  -> Effect Unit) -> QuoteListItemState -> PrestoDOM (Effect Unit) w
view push state =
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
                          _ <- launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ lift $ lift $ doAff do
                            if (os == "IOS") then liftEffect $ startTimerWithTime (show state.seconds) state.id "1" push CountDown
                              else liftEffect $ countDown state.seconds state.id push CountDown
                          pure unit
                        ) (const NoAction)
          , onClick push (const $ Click state)
          , disableClickFeedback true
          , padding (Padding 16 10 16 24)
          ][ linearLayout[
            orientation HORIZONTAL
          , width MATCH_PARENT
          , height WRAP_CONTENT
          ][  driverImageView state
            , linearLayout
              [ height WRAP_CONTENT
              , weight 1.0
              , padding (PaddingLeft 12)
              , orientation VERTICAL
              ][ nameAndPrice state push
               , horizontalLine state
               , ratingAndExpiryTime state push]]
               , primaryButtonView state push
              ]

nameAndPrice :: forall w . QuoteListItemState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
nameAndPrice state push =
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 ][ driverNameAndTimeView state
  , priceView state push
 ]

ratingAndExpiryTime :: forall w . QuoteListItemState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
ratingAndExpiryTime state push =
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , gravity CENTER_VERTICAL
 ][ driverRatingView state
  , timerView state push
 ]

driverImageView :: forall w . QuoteListItemState -> PrestoDOM (Effect Unit) w
driverImageView state =
 linearLayout
 [ width WRAP_CONTENT
 , height WRAP_CONTENT
 , orientation VERTICAL
 ][ frameLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity CENTER
    , margin (Margin 0 7 0 0)
    ][
      imageView
        [ margin (MarginLeft 27)
      , cornerRadius 18.0
      , background state.appConfig.quoteListItemConfig.driverImagebg
      , width (V 36)
      , height (V 36)
      , imageWithFallback ""

        ]
      , imageView
        [ height $ V state.appConfig.quoteListItemConfig.vehicleHeight
        , width $ V state.appConfig.quoteListItemConfig.vehicleWidth
        , cornerRadius 20.0
        , imageWithFallback if state.vehicleType == "auto" then "ny_ic_auto_quote_list," <> (getAssetStoreLink FunctionCall) <> "ny_ic_auto_quote_list.png" else "ny_ic_auto_quote_list," <> (getAssetStoreLink FunctionCall) <> "ny_ic_auto_quote_list.png"
        , weight 1.0
        ]
      ]
 ]

driverRatingView :: forall w . QuoteListItemState -> PrestoDOM (Effect Unit) w
driverRatingView state  =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation HORIZONTAL
    , gravity CENTER
    , cornerRadius 5.0
    , padding (PaddingRight 5)
    ][  imageView
        [ height $ V 13
        , width $ V 13
        , imageWithFallback $ "ny_ic_star_active," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_star_active.png"
        , margin (MarginRight 6)
        ]
      , textView (
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text $ if state.driverRating == 0.0 then (getString NEW_) else show $ state.driverRating
        ] <> FontStyle.tags LanguageStyle)
    ]


priceView :: forall w . QuoteListItemState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
priceView state push =
  linearLayout
    [ height MATCH_PARENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    , gravity CENTER_HORIZONTAL
    ][  textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text $ state.appConfig.currency <> " " <> state.price
        , color Color.black800
        , lineHeight "28"
        ] <> FontStyle.body10 TypoGraphy
    ]

timerView :: forall w . QuoteListItemState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
timerView state push =
 linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , gravity RIGHT
  , weight 1.0
  ][  textView (
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , text $ case (getValueToLocalStore LANGUAGE_KEY) of
            "EN_US" -> (getString EXPIRES_IN ) <>" : " <> state.timer <> "s"
            "FR_FR" -> (getString EXPIRES_IN ) <>" : " <> state.timer <> "s"
            _ -> state.timer <> "s " <> (getString EXPIRES_IN )
      , color state.appConfig.quoteListItemConfig.expiresColor
      , gravity CENTER
      ] <> FontStyle.body3 LanguageStyle)
  ]

driverNameAndTimeView :: forall w . QuoteListItemState -> PrestoDOM (Effect Unit) w
driverNameAndTimeView state =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    , weight 1.0
    , margin (Margin 0 5 20 10)
    ][  textView (
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text $ if state.timeLeft == 0 then (getString NEARBY) else show state.timeLeft <> (getString MINS_AWAY)
        , color Color.black800
        ] <> FontStyle.subHeading1 LanguageStyle)
      , textView (
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text state.driverName
        , color Color.black700
        ] <> FontStyle.tags LanguageStyle)
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
  ] <> if state.appConfig.isGradient == "true" then [gradient (Linear 90.0 state.appConfig.gradient)] else [background state.appConfig.primaryBackground])
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
            , text $ case (getValueToLocalStore LANGUAGE_KEY) of
                "EN_US" -> (getString AUTO_ACCEPTING_SELECTED_RIDE) <> " : " <> state.timer <> "s"
                _ -> state.timer <> "s " <> (getString AUTO_ACCEPTING_SELECTED_RIDE)
            ] <> FontStyle.tags LanguageStyle)
          , imageView
            [ height $ V 18
            , width $ V 18
            , imageWithFallback $ "ny_ic_close," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_close.png"
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
 , padding $ PaddingVertical 7 7
 , gravity CENTER
 ][ linearLayout
    [ width MATCH_PARENT
    , height $ V 1
    , background Color.grey
    ][]
 ]
