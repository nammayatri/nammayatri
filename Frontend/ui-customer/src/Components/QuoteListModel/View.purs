{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.QuoteListModel.View where

import Common.Types.App
import Animation (translateYAnimFromTop)
import Animation.Config (translateFullYAnimWithDurationConfig)
import Components.PrimaryButton as PrimaryButton
import Components.QuoteListItem as QuoteListItem
import Components.QuoteListModel.Controller (Action(..), QuoteListModelState)
import Components.SeparatorView.View as SeparatorView
import Data.Array (filter, head, null, (!!), mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag, isPreviousVersion, os, safeMarginBottom, safeMarginTop, screenWidth)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getAssetStoreLink, getAssetsBaseUrl, getCommonAssetStoreLink, getPaymentMethod)
import Helpers.Utils (getPreviousVersion)
import MerchantConfig.Utils (getMerchant, Merchant(..))
import JBridge (getBtnLoader, startLottieProcess, lottieAnimationConfig)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (getValueFromConfig)
import Prelude (Unit, show, bind, const, map, pure, unit, not, void, ($), (&&), (+), (/), (/=), (<<<), (<>), (==), (||), discard)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Accessiblity(..), PrestoDOM, Visibility(..), afterRender, accessibilityHint ,alignParentBottom, background, clickable, color, cornerRadius, ellipsize, fontStyle, gravity, height, id, imageUrl, imageView, imageWithFallback, lineHeight, linearLayout, lottieAnimationView, margin, onClick, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width, accessibility)
import PrestoDOM.Animation as PrestoAnim
import Screens.Types (Stage(..))
import Storage (KeyStore(..), getValueToLocalStore)
import Storage (isLocalStageOn)
import Styles.Colors as Color
import Data.String (replaceAll, Pattern(..), Replacement(..))

view :: forall w . (Action  -> Effect Unit) -> QuoteListModelState -> PrestoDOM (Effect Unit) w
view push state =
  PrestoAnim.animationSet [translateYAnimFromTop $ translateFullYAnimWithDurationConfig 500 ] $
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , accessibility DISABLE
  ][  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
       , accessibility DISABLE
      , clickable true
      ][ quoteListTopSheetView state push
        , selectRideAndConfirmView state push
        , linearLayout
          [ height $ V 1
          , width MATCH_PARENT
          , background Color.grey900
          , visibility if ( null state.quoteListModel) then GONE else VISIBLE
          ][]
        , quotesView state push
        ]
      --, primaryButtonView state push
      , paymentView state
    ]


paymentView :: forall w . QuoteListModelState -> PrestoDOM (Effect Unit) w
paymentView state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , visibility if state.selectedQuote == Nothing && (null state.quoteListModel) && (not isLocalStageOn FindingQuotes) && (not state.findingRidesAgain) then GONE else VISIBLE
  , alignParentBottom "true,-1"
  , background Color.white900
  , accessibility DISABLE
  , orientation VERTICAL
  ][  lottieAnimationView
          [ id (getNewIDWithTag "lottieLoaderAnimProgress")
          , afterRender (\action-> do
                        void $ pure $ startLottieProcess lottieAnimationConfig {rawJson = (getAssetsBaseUrl FunctionCall) <> "lottie/progress_loader_line.json", lottieId = (getNewIDWithTag "lottieLoaderAnimProgress"), minProgress = state.progress, scaleType = "CENTER_CROP"}
                        )(const NoAction)
          , height WRAP_CONTENT
          , width MATCH_PARENT
          , visibility if state.showProgress then VISIBLE else GONE
          , accessibilityHint "Booking Status: Looking for rides"
          , accessibility ENABLE
          ]
    , linearLayout
        [ background Color.grey900
        , height $ V 1
        , accessibility DISABLE
        , width MATCH_PARENT
        , visibility if ( null state.quoteListModel ) then GONE else VISIBLE
        ][]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      , accessibility DISABLE
      , padding $ PaddingVertical 15 (if safeMarginBottom == 0 then 17 else safeMarginBottom)
      , orientation HORIZONTAL
      ][  imageView
          [ imageWithFallback imageData.imageUrl
          , height imageData.height
          , width imageData.width
          , accessibility DISABLE
          , margin $ MarginRight 8
          , accessibility DISABLE 
          ]
        , textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ if (getPaymentMethod unit) == "cash" then (getString PAY_DRIVER_USING_CASH_OR_UPI) else (getString PAY_DRIVER_USING_WALLET)
          , gravity CENTER_HORIZONTAL
          , color Color.black800
          , accessibilityHint "Payment Method Cash or UPI"
          , accessibility ENABLE
          ] <> FontStyle.body1 TypoGraphy
        ]
    ]


imageData :: { height :: Length
, width :: Length
, imageUrl :: String
}
imageData = 
  if os == "IOS" then {imageUrl : "ny_ic_wallet_rect," <> (getAssetStoreLink FunctionCall) <> "ny_ic_wallet_rect.png", height : (V 15), width : (V 15)}
    else {imageUrl : "ny_ic_wallet," <> (getAssetStoreLink FunctionCall) <> "ny_ic_wallet.png", height : (V 24) , width : (V 24)}
    
---------------------------- sourceDestinationImageView ---------------------------------
sourceDestinationImageView :: forall w . QuoteListModelState -> PrestoDOM (Effect Unit) w
sourceDestinationImageView state =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , margin $ MarginTop 7
    , gravity CENTER
    , accessibility DISABLE
    , orientation VERTICAL
    ][ imageView
        [ height $ V 15
        , width $ V 15
        , accessibility DISABLE
        , imageWithFallback $ "ny_ic_pickup," <> (getAssetStoreLink FunctionCall) <> "ny_ic_pickup.png"
        ]
      , SeparatorView.view separatorConfig
      , imageView
        [ height $ V 15
        , width $ V 15
        , accessibility DISABLE
        , imageWithFallback $ "ny_ic_drop," <> (getAssetStoreLink FunctionCall) <> "ny_ic_drop.png"  
        ]
      ]

---------------------------- sourceDestinationEditTextView ---------------------------------
sourceDestinationView :: forall w . QuoteListModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
sourceDestinationView state push =
  linearLayout
    [ width MATCH_PARENT
    , orientation VERTICAL
    , height WRAP_CONTENT
    , margin $ MarginTop 7
    ][ linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity CENTER_VERTICAL
      ][ 
      imageView
        [ height $ V 15
        , width $ V 15
        , accessibility DISABLE
        , imageWithFallback $ "ny_ic_pickup," <> (getAssetStoreLink FunctionCall) <> "ny_ic_pickup.png"
        ] 
      , textView $
        [ height WRAP_CONTENT
        , margin $ MarginLeft 12
        , weight 1.0
        , text state.source
        , accessibility ENABLE
        , color state.appConfig.quoteListModel.textColor
        , accessibilityHint $ "Pickup Location is " <> (replaceAll (Pattern ",") (Replacement " : ") state.source)
        , ellipsize true
        , singleLine true
        ] <> FontStyle.paragraphText TypoGraphy
      ]
      , SeparatorView.view separatorConfig
      , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity CENTER_VERTICAL
      ][ 
        imageView
        [ height $ V 15
        , width $ V 15
        , accessibility DISABLE
        , imageWithFallback $ "ny_ic_drop," <> (getAssetStoreLink FunctionCall) <> "ny_ic_drop.png"  
        ]
        , textView $
        [ height WRAP_CONTENT
        , weight 1.0
        , text state.destination
        , margin $ MarginLeft 12
        , color state.appConfig.quoteListModel.textColor
        , accessibilityHint $ "Destination Location is " <>  (replaceAll (Pattern ",") (Replacement " : ") state.destination)
        , accessibility ENABLE
        , ellipsize true
        , singleLine true
        ] <> FontStyle.paragraphText TypoGraphy
      ]
    ]   
      
---------------------------- quotesView ---------------------------------
quotesView :: forall w . QuoteListModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
quotesView state push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility VISIBLE
    , accessibility DISABLE
    ][  quoteListView state push
      , noQuotesErrorModel state
      , findingRidesView state push
    ]

findingRidesView :: forall w . QuoteListModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
findingRidesView state push =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity CENTER_HORIZONTAL
  , visibility if (null state.quoteListModel && isLocalStageOn FindingQuotes) || state.findingRidesAgain then VISIBLE else GONE
  , clickable true
  , accessibility DISABLE
  , margin $ if state.tipViewProps.onlyPrimaryText then MarginBottom 80 else if state.tipViewProps.isprimaryButtonVisible then MarginBottom 82 else  MarginBottom 85
  , orientation VERTICAL
  ][
    linearLayout
    [ width MATCH_PARENT
    , orientation VERTICAL
    , weight 1.0
    , gravity CENTER
    , accessibility DISABLE
    ]
    [
      lottieAnimationView
      [ id (getNewIDWithTag "lottieLoaderAnim")
      , afterRender (\action-> do
                    void $ pure $ startLottieProcess lottieAnimationConfig{ rawJson = if (state.appConfig.autoVariantEnabled && state.vehicleVariant == "AUTO_RICKSHAW") then (getAssetsBaseUrl FunctionCall) <> "lottie/finding_rides_loader_with_text_auto.json" else (getAssetsBaseUrl FunctionCall) <> "lottie/finding_rides_loader_with_text.json", lottieId = (getNewIDWithTag "lottieLoaderAnim") }
                    pure unit)(const NoAction)
      , height $ V state.appConfig.quoteListModel.lottieHeight
      , accessibility DISABLE
      , width $ V state.appConfig.quoteListModel.lottieWidth
      ]
    , textView 
      [ text (getString PLEASE_WAIT)
      , color "#7C7C7C"
      , visibility if state.appConfig.showQuoteFindingText then VISIBLE else GONE
      , textSize FontSize.a_17
      , margin $ MarginTop if state.appConfig.showQuoteFindingText then 22 else 0
      , accessibility DISABLE
      , lineHeight "25"
      , fontStyle $ FontStyle.regular LanguageStyle
      ]
    , textView 
      [ text (getString FINDING_QUOTES_TEXT)
      , color "#7C7C7C"
      , visibility if state.appConfig.showQuoteFindingText then VISIBLE else GONE
      , textSize FontSize.a_17
      , accessibility DISABLE
      , lineHeight "25"
      , fontStyle $ FontStyle.regular LanguageStyle
      ]
    ]
  , addTipView state push
  ]

addTipView :: forall w . QuoteListModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
addTipView state push =
  linearLayout
    [ width MATCH_PARENT
    , orientation VERTICAL
    , height WRAP_CONTENT
    , visibility if state.tipViewProps.isVisible then VISIBLE else GONE
    , accessibility DISABLE
    ]
    [
      linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , alignParentBottom "true,-1"
        , background Color.pink
        , margin $ MarginHorizontal 16 16
        , cornerRadius 12.0
        , padding $ Padding 20 16 20 16
        , accessibility DISABLE
        ]
        [
          textView
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , text state.tipViewProps.secondaryText
          , color Color.black800
          , gravity CENTER
          , textSize $ FontSize.a_12
          , accessibility ENABLE
          , accessibilityHint state.tipViewProps.secondaryText
          , fontStyle $ FontStyle.regular LanguageStyle
          , visibility if state.tipViewProps.onlyPrimaryText then GONE else VISIBLE
          ]
        , textView
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , text state.tipViewProps.primaryText
          , color Color.black800
          , gravity CENTER
          , accessibility ENABLE
          , accessibilityHint state.tipViewProps.primaryText
          , textSize $ FontSize.a_14
          , fontStyle $ FontStyle.bold LanguageStyle
          ]
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , margin (MarginTop  16)
          , visibility if state.tipViewProps.onlyPrimaryText then GONE else VISIBLE
          ]
          ( mapWithIndex
              ( \index item ->
                  linearLayout
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , weight if index == 2 then 0.0 else 1.0
                    ]
                    [ textView
                        [ text $ item
                        , color $ Color.black800
                        , textSize FontSize.a_14
                        , stroke $ "1," <> (if (state.tipViewProps.activeIndex == index) then Color.blue800 else Color.grey900)
                        , cornerRadius 8.0
                        , width WRAP_CONTENT
                        , height WRAP_CONTENT
                        , accessibility ENABLE
                        , padding (Padding 20 10 20 10)
                        , accessibilityHint $ "₹" <> show (fromMaybe 100 (state.tipViewProps.customerTipArrayWithValues !! index)) <> " Tip"<> (if (state.tipViewProps.activeIndex == index) then " Selected" else " : Button")
                        , fontStyle $ FontStyle.bold LanguageStyle
                        , onClick push $ const $ TipBtnClick index (fromMaybe 100 (state.tipViewProps.customerTipArrayWithValues !! index))
                        , background $ if state.tipViewProps.activeIndex == index then Color.blue600 else Color.white900
                        ]
                    ]
              )state.tipViewProps.customerTipArray
          )
        , linearLayout
          [
            width MATCH_PARENT
          , height WRAP_CONTENT
          , visibility if state.tipViewProps.isprimaryButtonVisible && not state.tipViewProps.onlyPrimaryText then VISIBLE else GONE
          ][
            PrimaryButton.view (push <<< TipViewPrimaryButtonClick) (continueWithTipButtonConfig state)
          ]
        ]
    ]

selectRideAndConfirmView :: forall w . QuoteListModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
selectRideAndConfirmView state push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , background Color.white900
  , visibility if ( null state.quoteListModel ) then GONE else VISIBLE
  ][ linearLayout[
    height WRAP_CONTENT
  , width WRAP_CONTENT
  , weight 1.0
  , padding (Padding 16 16 0 16)
  -- , background state.appConfig.quoteListModel.backgroundColor
  ][textView (
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , color state.appConfig.quoteListModel.selectRideTextColor
    , text case getValueToLocalStore AUTO_SELECTING of
       "CANCELLED_AUTO_ASSIGN" -> "Select a Ride"
       "false"                 -> "Select a Ride"
       _                       -> case (getValueToLocalStore LANGUAGE_KEY) of
                                    _ -> "Confirming selected ride in" <> " : " <> (fromMaybe dummyQuoteList ((filter (\item -> item.id == (fromMaybe "" state.selectedQuote)) state.quoteListModel) !! 0)).timer <> "s"
                                    -- _ -> "state.timer" <> "s " <> (getString AUTO_ACCEPTING_SELECTED_RIDE) TODO :: NEED TO UPDATE LANGUAGE
    ] <> FontStyle.subHeading2 TypoGraphy)]
   , linearLayout
    [ height MATCH_PARENT
    , width WRAP_CONTENT
    , gravity CENTER
    , padding (Padding 64 12 16 12)
    , onClick push $ const CancelAutoAssigning
    ][ imageView
      [ height $ V 24
      , width $ V 24
      , imageWithFallback $ "ny_ic_close," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_close.png"
      , visibility if getValueToLocalStore AUTO_SELECTING == "false" || getValueToLocalStore AUTO_SELECTING == "CANCELLED_AUTO_ASSIGN" then GONE else VISIBLE 
     
      ]
    ]
  ]

paymentMethodView :: forall w.(Action -> Effect Unit) ->  QuoteListModelState -> PrestoDOM (Effect Unit) w
paymentMethodView push state =
  linearLayout[
    orientation HORIZONTAL
  , width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER_VERTICAL
  , visibility if state.selectedQuote == Nothing && (null state.quoteListModel) && (not isLocalStageOn FindingQuotes) then VISIBLE else GONE
  ][linearLayout
  [ orientation VERTICAL
  , height WRAP_CONTENT
  , padding (Padding 16 6 16 16)
  , width WRAP_CONTENT
  , gravity LEFT
  ][  textView (
      [ text (getString PAYMENT_METHOD)
      , color Color.black700
      ] <> FontStyle.body3 TypoGraphy)
    , linearLayout
      [ orientation HORIZONTAL
      , width WRAP_CONTENT
      , height WRAP_CONTENT
      , margin (MarginTop 7)
      ][  imageView
          [ imageWithFallback $ "ny_ic_wallet," <> (getAssetStoreLink FunctionCall) <> "ny_ic_wallet.png"
          , height $ V 20
          , width $ V 20
          , accessibility DISABLE
          ]
        , textView $
          [ text (getString PAYMENT_METHOD_STRING)
          , margin (MarginLeft 8)
          , color Color.black800
          ] <> FontStyle.body1 TypoGraphy
      ]
  ] -- TODO ADD PAYMENT OPTIONS
  -- , linearLayout[
  --   height WRAP_CONTENT
  -- , width MATCH_PARENT
  -- , gravity RIGHT
  -- , padding (PaddingRight 16)
  -- ][imageView
  --   [ imageUrl "ic_chevron_right"
  --   , height $ V 20
  --   , width $ V 20
  --   , gravity RIGHT
  --   ]]
  ]

---------------------------- quoteListTopSheetView ---------------------------------
quoteListTopSheetView :: forall w . QuoteListModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
quoteListTopSheetView state push =
   linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , background state.appConfig.quoteListModel.backgroundColor
      , accessibility DISABLE
      , padding $ PaddingTop safeMarginTop
      ][  linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation HORIZONTAL
          , gravity CENTER_VERTICAL
          , padding $ Padding 10 10 16 28
          ][ linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation HORIZONTAL
              ][ linearLayout
                  [ height $ V 40
                  , width $ V 40
                  , onClick push $ const GoBack
                  , accessibilityHint "Cancel Search : Button"
                  , accessibility ENABLE
                  ][  imageView
                      [ height $ V 24
                      , width $ V 24
                      , accessibility DISABLE
                      , imageWithFallback state.appConfig.quoteListModel.closeIcon
                      , margin $ MarginTop 7
                      ]
                  ]
                , sourceDestinationView state push
                ]
            ]
        ]

noQuotesErrorModel :: forall w . QuoteListModelState -> PrestoDOM (Effect Unit) w
noQuotesErrorModel state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , background Color.white900
    , accessibility DISABLE
    , visibility if ( null state.quoteListModel) && isLocalStageOn QuoteList then VISIBLE else GONE
    , margin (MarginBottom 100)
    ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER
      ][imageView
        [ height $ V 115
        , width $ V 161
        , accessibility DISABLE
        , imageWithFallback $ if state.vehicleVariant == "AUTO_RICKSHAW" 
                                then "ny_ic_no_quotes_auto," <> getAssetStoreLink FunctionCall  <> "ny_ic_no_quotes_auto.png"
                                else "ny_ic_no_quotes_color," <> getAssetStoreLink FunctionCall  <> "ny_ic_no_quotes_color.png"
        ]
      , textView $
        [ height WRAP_CONTENT
        , width $ V ((screenWidth unit / 2) + (screenWidth unit /3))
        , color Color.black800
        , text (getString SORRY_WE_COULDNT_FIND_ANY_RIDES)
        , margin $ MarginVertical 20 4
        , gravity CENTER
        ] <> FontStyle.h2 TypoGraphy
      , textView $
        [ height WRAP_CONTENT
        , width $ V ((screenWidth unit / 2) + (screenWidth unit /3))
        , text (getString IT_SEEMS_TO_BE_A_VERY_BUSY_DAY)
        , color Color.black700
        , gravity CENTER
        ] <> FontStyle.paragraphText TypoGraphy
    ]
    ]


---------------------------- quoteListView ---------------------------------
quoteListView :: forall w . QuoteListModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
quoteListView state push =
  scrollView
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , scrollBarY false
    , visibility if ( null state.quoteListModel) then GONE else VISIBLE
    ][  linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , accessibility DISABLE
        , orientation VERTICAL
        ](map (\item ->
            QuoteListItem.view (push <<< QuoteListItemActionController) item{appConfig = state.appConfig}) state.quoteListModel)
    ]

---------------------------- primaryButtonView ---------------------------------
primaryButtonView :: forall w . QuoteListModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
primaryButtonView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , weight 1.0
    , alignParentBottom "true,-1"
    , background Color.white900 -- TODO : change to white900 once shadow is fixed
    -- --, visibility GONE-- $ checkVisibility state
    , padding (Padding 0 16 0 30)
    ][ homeOrTryAgain state push ]

---------------------------- homeOrTryAgainView ---------------------------------
homeOrTryAgain :: forall w . QuoteListModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
homeOrTryAgain state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , visibility if state.selectedQuote == Nothing && (null state.quoteListModel) && isLocalStageOn QuoteList then VISIBLE else GONE
    ][ PrimaryButton.view (push <<< HomeButtonActionController) (homeButtonConfig state)
     , PrimaryButton.view (push <<< TryAgainButtonActionController) (tryAgainButtonConfig state)
    ]

-- buttonView :: forall w. QuoteListModelState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
-- buttonView state push =
--   linearLayout
--   [ height WRAP_CONTENT
--   , width MATCH_PARENT
--   , margin (MarginHorizontal 16 16)
--   , background Color.black900
--   , padding (PaddingHorizontal 14 14)
--   , gravity CENTER
--   , onClick push (const HidePopUp)
--   ][ textView
--       [ text "Got It!"
--       , textSize FontSize.a_16
--       , color Color.yellow900
--       , gravity CENTER
--       ]
--     ]

---------------------------- continueWithTipButtonConfig ---------------------------------
continueWithTipButtonConfig :: QuoteListModelState -> PrimaryButton.Config
continueWithTipButtonConfig state = let
    config = PrimaryButton.config
    continueWithTipButtonConfig' = config
      { textConfig
        { text = state.tipViewProps.primaryButtonText
        , accessibilityHint = state.tipViewProps.primaryButtonText <> " : Button" 
        }
      , id = "ContinueWithTipButtonQuoteList"
      , margin = MarginTop 10
      }
  in continueWithTipButtonConfig'

---------------------------- homeButtonConfig ---------------------------------
homeButtonConfig :: QuoteListModelState -> PrimaryButton.Config
homeButtonConfig state = let
    config = PrimaryButton.config
    homeButtonConfig' = config
      { textConfig
        { text = (getString HOME)
        , color = Color.black900
        }
      , margin =( Margin 16 0 8 0)
      , width = V $ (screenWidth unit/4)
      , background = Color.white900
      , stroke = ("1," <> Color.black)
      , id = "HomeButtonQuoteList"
      , enableLoader = (getBtnLoader "HomeButtonQuoteList")
      }
  in homeButtonConfig'

---------------------------- tryAgainButtonConfig ---------------------------------
tryAgainButtonConfig :: QuoteListModelState -> PrimaryButton.Config
tryAgainButtonConfig state = let
    config = PrimaryButton.config
    tryAgainButtonConfig' = config
      { textConfig
        { text = (getString TRY_AGAIN)
        ,  color = state.appConfig.primaryTextColor 
        }
      , margin =( Margin 8 0 16 0)
      , width = MATCH_PARENT
      , id = "TryAgainButtonQuoteList"
      , enableLoader = (getBtnLoader "TryAgainButtonQuoteList")
      , background = state.appConfig.primaryBackground
      }
  in tryAgainButtonConfig'


getPrice :: QuoteListModelState -> String
getPrice state =
  let selectQuoteArray = (filter (\x -> state.selectedQuote == Just x.id) state.quoteListModel)
      price = (fromMaybe dummyQuoteList (head selectQuoteArray)).price
    in price



dummyQuoteList :: QuoteListItem.QuoteListItemState
dummyQuoteList = QuoteListItem.config{
   seconds = 15
  , id = ""  
  , timer = ""
  , timeLeft = 15
  , driverRating = 0.0
  , profile = ""
  , price = ""
  , vehicleType = "auto"
  , driverName = ""
  , selectedQuote = Nothing
  }


checkVisibility :: QuoteListModelState -> Visibility
checkVisibility state =
  case state.selectedQuote ,(null state.quoteListModel) of
    Just _ ,_                           -> VISIBLE
    Nothing , true                      -> VISIBLE
    Nothing , false                     -> GONE

setText :: QuoteListModelState -> String
setText state =
  case state.selectedQuote ,(null state.quoteListModel) of
    Just _ ,_                           -> (getString CONFIRM_FOR) <> (getValueFromConfig "currency") <> " " <> getPrice state
    Nothing , true                      -> (getString GO_HOME_)
    _,_                                 -> ""

getSelectedItemTimer :: QuoteListModelState -> String
getSelectedItemTimer state =
  let selectQuoteArray = (filter (\x -> state.selectedQuote == Just x.id) state.quoteListModel)
      timer = (fromMaybe dummyQuoteList (head selectQuoteArray)).timer
    in timer

separatorConfig :: SeparatorView.Config
separatorConfig = 
  {
    orientation : VERTICAL
  , count : 3
  , height : V 4
  , width : V 2
  , layoutWidth : V 12
  , layoutHeight : V 15
  }