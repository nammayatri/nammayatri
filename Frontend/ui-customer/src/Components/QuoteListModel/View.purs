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
import Data.Array (filter, head, null, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag, isPreviousVersion, os, safeMarginBottom, safeMarginTop, screenWidth)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink, getAssetsBaseUrl)
import Helpers.Utils (getPreviousVersion)
import JBridge (getBtnLoader, startLottieProcess)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((<>))
import Prelude (Unit, bind, const, map, pure, unit, ($), (&&), (+), (/), (/=), (<<<), (<>), (==), (||))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, alignParentBottom, background, clickable, color, ellipsize, fontStyle, gravity, height, id, imageUrl, imageView, imageWithFallback, lineHeight, linearLayout, lottieAnimationView, margin, onClick, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, text, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import MerchantConfig.Utils (getValueFromConfig)

view :: forall w . (Action  -> Effect Unit) -> QuoteListModelState -> PrestoDOM (Effect Unit) w
view push state =
  PrestoAnim.animationSet [translateYAnimFromTop $ translateFullYAnimWithDurationConfig 500 ] $
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
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
  , visibility if (state.selectedQuote == Nothing && (null state.quoteListModel) && getValueToLocalStore LOCAL_STAGE /= "FindingQuotes" ) then GONE else VISIBLE
  , alignParentBottom "true,-1"
  , background Color.white900
  , orientation VERTICAL
  ][  lottieAnimationView
          [ id (getNewIDWithTag "lottieLoaderAnimProgress")
          , afterRender (\action-> do
                        _ <- pure $ startLottieProcess ((getAssetsBaseUrl FunctionCall) <> "lottie/progress_loader_line.json") (getNewIDWithTag "lottieLoaderAnimProgress") true 0.6 "CENTER_CROP"
                        pure unit)(const NoAction)
          , height WRAP_CONTENT
          , width MATCH_PARENT
          , visibility if state.showProgress then VISIBLE else GONE
          ]
    , linearLayout
        [ background Color.grey900
        , height $ V 1
        , width MATCH_PARENT
        , visibility if ( null state.quoteListModel ) then GONE else VISIBLE
        ][]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      , padding (PaddingVertical 15 (if safeMarginBottom == 0 then 17 else safeMarginBottom))
      , orientation HORIZONTAL
      ][  imageView
          [ imageWithFallback imageData.imageUrl
          , height imageData.height
          , width imageData.width
          , margin (MarginRight 8)
          ]
        , textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text (getString PAY_DRIVER_USING_CASH_OR_UPI)
          , gravity CENTER_HORIZONTAL
          , color Color.black800
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
    [ height MATCH_PARENT
    , width WRAP_CONTENT
    , margin (MarginTop 7)
    , gravity CENTER
    , orientation VERTICAL
    ][ imageView
        [ height $ V 15
        , width $ V 15
        , imageWithFallback $ "ny_ic_pickup," <> (getAssetStoreLink FunctionCall) <> "ny_ic_pickup.png"
        ]
      , imageView
        [ height $ V 27
        , width $ V 15
        , imageUrl if os == "IOS" then (if isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion "") then "ic_line_img" else "ny_ic_line_img") else state.appConfig.quoteListModel.lineImage
        , margin if os == "IOS" then (Margin 0 0 0 0) else (Margin 7 0 0 0)
        ]
      , imageView
        [ height $ V 15
        , width $ V 15
        , imageWithFallback $ "ny_ic_drop," <> (getAssetStoreLink FunctionCall) <> "ny_ic_drop.png"  
        ]
      ]

---------------------------- sourceDestinationEditTextView ---------------------------------
sourceDestinationTextView :: forall w . QuoteListModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
sourceDestinationTextView state push =
  linearLayout
    [ width MATCH_PARENT
    , orientation VERTICAL
    , height WRAP_CONTENT
    , margin (MarginTop 5)
    ][   
      textView (
        [ height WRAP_CONTENT
        , weight 1.0
        , text state.source
        , color state.appConfig.quoteListModel.textColor
        , padding (PaddingHorizontal 5 5)
        , margin (MarginBottom 12)
        , ellipsize true
        , singleLine true
        ] <> FontStyle.paragraphText TypoGraphy)
      , textView (
        [ height WRAP_CONTENT
        , weight 1.0
        , text state.destination
        , color state.appConfig.quoteListModel.textColor
        , padding (PaddingHorizontal 5 5)
        , margin (MarginTop 12)
        , ellipsize true
        , singleLine true
        ] <> FontStyle.paragraphText TypoGraphy)
    ]   
      
---------------------------- quotesView ---------------------------------
quotesView :: forall w . QuoteListModelState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
quotesView state push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility VISIBLE
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
  , visibility if null state.quoteListModel && getValueToLocalStore LOCAL_STAGE == "FindingQuotes" then VISIBLE else GONE
  , margin $ MarginTop state.appConfig.quoteListModel.topMargin
  , clickable true
  , orientation VERTICAL
  ][
    lottieAnimationView
      [ id $ getNewIDWithTag "1234567893"
      , afterRender (\action-> do
                    _ <- pure $ startLottieProcess ((getAssetsBaseUrl FunctionCall) <> "lottie/finding_rides_loader_with_text.json") (getNewIDWithTag "1234567893") true 0.6 "Default"
                    pure unit)(const NoAction)
      , height $ V state.appConfig.quoteListModel.lottieHeight
      , width $ V state.appConfig.quoteListModel.lottieWidth
      ]
  , textView 
    [ text (getString PLEASE_WAIT)
    , color "#7C7C7C"
    , visibility if state.appConfig.showQuoteFindingText then VISIBLE else GONE
    , textSize FontSize.a_17
    , margin $ MarginTop 22
    , lineHeight "25"
    , fontStyle $ FontStyle.regular LanguageStyle
    ]
  , textView 
    [ text (getString FINDING_QUOTES_TEXT)
    , color "#7C7C7C"
    , visibility if state.appConfig.showQuoteFindingText then VISIBLE else GONE
    , textSize FontSize.a_17
    , lineHeight "25"
    , fontStyle $ FontStyle.regular LanguageStyle
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
  , visibility if (state.selectedQuote == Nothing && (null state.quoteListModel) && getValueToLocalStore LOCAL_STAGE /= "FindingQuotes" ) then VISIBLE else GONE
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
      , padding $ PaddingTop safeMarginTop
      ][  linearLayout
          [ height MATCH_PARENT
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
                  ][  imageView
                      [ height $ V 24
                      , width $ V 24
                      , imageWithFallback $ if state.appConfig.nyBrandingVisibility then  "ny_ic_close," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_close.png" else "ny_ic_close_white," <> (getAssetStoreLink FunctionCall) <> "ny_ic_close_white.png"
                      , margin $ MarginTop 7
                      ]
                  ]
                , sourceDestinationImageView state
                , sourceDestinationTextView state push
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
    , visibility if ( null state.quoteListModel) && getValueToLocalStore LOCAL_STAGE == "QuoteList" then VISIBLE else GONE
    , margin (MarginBottom 100)
    ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER
      ][imageView
        [ height $ V state.appConfig.quoteListModel.noQuotesImageHeight
        , width $ V state.appConfig.quoteListModel.noQuotesImageWidth
        , imageWithFallback $ "ny_ic_no_quotes," <> (getAssetStoreLink FunctionCall) <> "ny_ic_no_quotes.png"
        , padding (Padding 0 0 0 0)
        ]
      , textView $
        [ height WRAP_CONTENT
        , width $ V ((screenWidth unit / 2) + (screenWidth unit /3))
        , color Color.black800
        , text (getString SORRY_WE_COULDNT_FIND_ANY_RIDES)
        , margin (Margin 0 32 0 4)
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
    , visibility if (state.selectedQuote == Nothing && (null state.quoteListModel) && getValueToLocalStore LOCAL_STAGE == "QuoteList" ) then VISIBLE else GONE
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