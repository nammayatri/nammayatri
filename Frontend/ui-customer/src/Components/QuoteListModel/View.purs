{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.QuoteListModel.View where

import Animation (translateYAnimFromTop)
import Animation.Config (translateFullYAnimWithDurationConfig)
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.QuoteListItem as QuoteListItem
import Components.QuoteListModel.Controller (Action(..), QuoteListModelState)
import Data.Array (filter, head, null, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag, os, safeMarginTop, screenWidth,safeMarginBottom)
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge (getBtnLoader, startLottieProcess)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, map, pure, unit, ($), (&&), (+), (/), (/=), (<<<), (<>), (==), (||))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, alignParentBottom, background, clickable, color, ellipsize, fontStyle, gravity, height, id, imageUrl, imageView, linearLayout, lottieAnimationView, margin, onClick, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, text, textSize, textView, visibility, weight, width, imageWithFallback)
import PrestoDOM.Animation as PrestoAnim
import Storage (KeyStore(..), getValueToLocalStore)
import Helpers.Utils (isPreviousVersion, getPreviousVersion)
import Styles.Colors as Color
import Common.Types.App

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
      , primaryButtonView state push 
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
                      _ <- pure $ startLottieProcess "progress_loader_line" (getNewIDWithTag "lottieLoaderAnimProgress") true 0.6 "CENTER_CROP"
                      pure unit)(const NoAction)
        , height WRAP_CONTENT
        , visibility if ( (null state.quoteListModel)  && getValueToLocalStore LOCAL_STAGE == "FindingQuotes") then VISIBLE else GONE
        , width MATCH_PARENT
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
          , textSize FontSize.a_14 
          , fontStyle $ FontStyle.medium LanguageStyle
          ]
        ]
    ]


imageData :: { height :: Length
, width :: Length
, imageUrl :: String
}
imageData = 
  if os == "IOS" then {imageUrl : "ny_ic_wallet_rect,https://assets.juspay.in/nammayatri/images/user/ny_ic_wallet_rect.png", height : (V 15), width : (V 15)}
    else {imageUrl : "ny_ic_wallet,https://assets.juspay.in/nammayatri/images/user/ny_ic_wallet.png", height : (V 24) , width : (V 24)}
    
---------------------------- sourceDestinationImageView ---------------------------------
sourceDestinationImageView :: forall w . PrestoDOM (Effect Unit) w
sourceDestinationImageView  = 
  linearLayout
    [ height MATCH_PARENT
    , width WRAP_CONTENT
    , margin (MarginTop 7)
    , gravity CENTER 
    , orientation VERTICAL
    ][ imageView
        [ height $ V 15
        , width $ V 15
        , imageWithFallback "ny_ic_pickup,https://assets.juspay.in/nammayatri/images/user/ny_ic_pickup.png"
        ]
      , imageView
        [ height $ V 27
        , width $ V 15
        , imageUrl if os == "IOS" then (if isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion "") then "ic_line_img" else "ny_ic_line_img") else "ic_line"
        , margin if os == "IOS" then (Margin 0 0 0 0) else (Margin 7 0 0 0)
        ]
      , imageView
        [ height $ V 15
        , width $ V 15
        , imageWithFallback "ny_ic_drop,https://assets.juspay.in/nammayatri/images/user/ny_ic_drop.png"  
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
      textView
        [ height WRAP_CONTENT
        , weight 1.0
        , text state.source
        , color Color.white900
        , fontStyle $ FontStyle.regular LanguageStyle
        , padding (PaddingHorizontal 5 5)
        , margin (MarginBottom 12)
        , textSize FontSize.a_14
        , ellipsize true
        , singleLine true
        ]
      , textView
        [ height WRAP_CONTENT
        , weight 1.0
        , text state.destination
        , color Color.white900
        , fontStyle $ FontStyle.regular LanguageStyle
        , padding (PaddingHorizontal 5 5)
        , margin (MarginTop 12)
        , textSize FontSize.a_14
        , ellipsize true
        , singleLine true
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
  , margin $ MarginTop 100
  , clickable true
  ][
    lottieAnimationView 
      [ id (getNewIDWithTag "lottieLoaderAnim")
      , afterRender (\action-> do
                    _ <- pure $ startLottieProcess "finding_rides_loader_with_text" (getNewIDWithTag "lottieLoaderAnim") true 0.6 "Default"
                    pure unit)(const NoAction)
      , height $ V 300
      , width $ V 300
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
  , weight 1.0
  , padding (Padding 16 16 0 16)
  ][textView
    [ height WRAP_CONTENT
    , color Color.black900
    , textSize FontSize.a_16
    , fontStyle $ FontStyle.medium LanguageStyle
    , text case getValueToLocalStore AUTO_SELECTING of
       "CANCELLED_AUTO_ASSIGN" -> "Select a Ride"
       "false"                 -> "Select a Ride"
       _                       -> case (getValueToLocalStore LANGUAGE_KEY) of
                                    _ -> "Confirming selected ride in" <> " : " <> (fromMaybe configDummy ((filter (\item -> item.id == (fromMaybe "" state.selectedQuote)) state.quoteListModel) !! 0)).timer <> "s"
                                    -- _ -> "state.timer" <> "s " <> (getString AUTO_ACCEPTING_SELECTED_RIDE) TODO :: NEED TO UPDATE LANGUAGE
    ]]
   , linearLayout
    [ height MATCH_PARENT
    , width WRAP_CONTENT
    , gravity CENTER
    , padding (Padding 16 12 16 12)
    , onClick push $ const CancelAutoAssigning
    ][ imageView
      [ height $ V 24
      , width $ V 24
      , imageWithFallback "ny_ic_close,https://assets.juspay.in/nammayatri/images/common/ny_ic_close.png"
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
  ][  textView
      [ text (getString PAYMENT_METHOD)
      , textSize FontSize.a_12
      , color Color.black700
      , fontStyle $ FontStyle.regular LanguageStyle
      ]
    , linearLayout
      [ orientation HORIZONTAL
      , width WRAP_CONTENT
      , height WRAP_CONTENT
      , margin (MarginTop 7)
      ][  imageView
          [ imageWithFallback "ny_ic_wallet,https://assets.juspay.in/nammayatri/images/user/ny_ic_wallet.png"
          , height $ V 20
          , width $ V 20
          ]
        , textView
          [ text (getString PAYMENT_METHOD_STRING)
          , margin (MarginLeft 8)
          , textSize FontSize.a_14
          , color Color.black800
          , fontStyle $ FontStyle.medium LanguageStyle
          ]
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
      , background Color.black900
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
                      , imageWithFallback "ny_ic_close_white,https://assets.juspay.in/nammayatri/images/user/ny_ic_close_white.png"
                      , margin $ MarginTop 7
                      ]
                  ]
                , sourceDestinationImageView 
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
        [ height $ V 115
        , width $ V 137
        , imageWithFallback "ic_no_quotes,https://assets.juspay.in/nammayatri/images/user/ny_ic_no_quotes.png"
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
            QuoteListItem.view (push <<< QuoteListItemActionController) item) state.quoteListModel)
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
    -- , visibility GONE-- $ checkVisibility state
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
        , textSize = FontSize.a_16  
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
        , color = Color.yellow900      
        , textSize = FontSize.a_16  
        }
      , margin =( Margin 8 0 16 0)
      , width = MATCH_PARENT
      , id = "TryAgainButtonQuoteList"
      , enableLoader = (getBtnLoader "TryAgainButtonQuoteList")
      }
  in tryAgainButtonConfig'

configDummy :: QuoteListItem.QuoteListItemState
configDummy = {
   seconds : 15
  , id : ""  
  , timer : "-"
  , timeLeft : 0
  , driverRating : 4.0
  , profile : ""
  , price : "0"
  , vehicleType : "auto"
  , driverName : "Drive_Name"
  , selectedQuote : Nothing
  }


getPrice :: QuoteListModelState -> String
getPrice state =  
  let selectQuoteArray = (filter (\x -> state.selectedQuote == Just x.id) state.quoteListModel)
      price = (fromMaybe dummyQuoteList (head selectQuoteArray)).price
    in price 



dummyQuoteList :: QuoteListItem.QuoteListItemState
dummyQuoteList = {
   seconds : 15
  , id : ""  
  , timer : ""
  , timeLeft : 15
  , driverRating : 0.0
  , profile : ""
  , price : ""
  , vehicleType : "auto"
  , driverName : ""
  , selectedQuote : Nothing
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
    Just _ ,_                           -> (getString CONFIRM_FOR) <> "₹ " <> getPrice state
    Nothing , true                      -> (getString GO_HOME_)
    _,_                                 -> ""

getSelectedItemTimer :: QuoteListModelState -> String
getSelectedItemTimer state =  
  let selectQuoteArray = (filter (\x -> state.selectedQuote == Just x.id) state.quoteListModel)
      timer = (fromMaybe dummyQuoteList (head selectQuoteArray)).timer
    in timer 