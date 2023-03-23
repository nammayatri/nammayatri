{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.CancelRide.View where

import Prelude
import PrestoDOM (Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..), Length(..), PrestoDOM, background, clickable, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, orientation, text, textSize, textView, weight, width, padding, visibility, frameLayout, stroke, scrollView, afterRender, editText, onClick, id, onChange, pattern, relativeLayout, alignParentBottom, adjustViewWithKeyboard, singleLine, hint, hintColor, multiLineEditText, disableClickFeedback, imageWithFallback )
import Effect (Effect)
import PrestoDOM.Properties (lineHeight, cornerRadii)
import PrestoDOM.Types.DomAttributes (Gravity(..), Corners(..))
import Components.CancelRide.Controller
import Components.PrimaryButton.Controller as PrimaryButtonConfig
import Components.PrimaryButton.View as PrimaryButton
import Styles.Colors as Color
import Font.Style as FontStyle
import Font.Size as FontSize
import Data.Array (mapWithIndex, length)
import Engineering.Helpers.Commons (screenWidth)
import Log (printLog)
import Language.Types(STR(..))
import Effect.Aff (launchAff_)
import Engineering.Helpers.Commons (flowRunner, os, setText')
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Services.Backend as Remote
import Control.Monad.Trans.Class (lift)
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Data.Array (length,(!!))
import Data.Maybe
import Common.Types.App
import JBridge(requestKeyboardShow, hideKeyboardOnNavigation)
import Constant.Test as Id

view :: forall w .  (Action  -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  relativeLayout
    [ width MATCH_PARENT
      , height MATCH_PARENT
      , clickable true
      , background Color.black9000
      , disableClickFeedback true
      , gravity BOTTOM
      , onClick push (const OnGoBack)
      , adjustViewWithKeyboard "true"
      , Id.testId $ Id.Component (Id.cancelRide <> Id.cancel)
      ][ linearLayout 
          [ width MATCH_PARENT
          , alignParentBottom "true,-1"
          , height WRAP_CONTENT
          , clickable true
          , disableClickFeedback true
          , onClick ( \action -> do 
            _ <- pure $ hideKeyboardOnNavigation true
            pure unit 
          ) (const NoAction)
          , cornerRadii $ Corners 20.0 true true false false
          , orientation VERTICAL
          , background Color.white900
          , padding (Padding 20 20 20 0)
          ][  headingText config push , 
              cancelationReasonsList push config
           ]
         ]


headingText :: forall w . Config -> (Action -> Effect Unit) ->  PrestoDOM (Effect Unit) w
headingText config push = 
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , orientation VERTICAL
 , clickable true
 , onClick ( \action -> do 
            _ <- pure $ hideKeyboardOnNavigation true
            pure unit 
          ) (const NoAction)
 , disableClickFeedback true
 ][ textView
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , text config.headingText
    , color Color.black800
    , orientation HORIZONTAL
    , gravity CENTER
    , textSize FontSize.a_22
    , fontStyle $ FontStyle.bold LanguageStyle
    ], 
    textView 
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , text config.subHeadingText
    , orientation HORIZONTAL
    , padding (PaddingTop 4)
    , gravity CENTER
    , textSize FontSize.a_14
    , color Color.black700
    , fontStyle $ FontStyle.regular LanguageStyle
    ],
    linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , padding (Padding 4 12 4 12)
    , margin (Margin 2 16 0 0)
    , orientation VERTICAL
    , onClick ((\action -> do
        _ <- push action
        _ <- setText' (getNewIDWithTag "OtherReasonEditText") ""
        _ <- setText' (getNewIDWithTag "TechGlitchEditText") ""
        pure unit 
    )) ( const ClearOptions)
    , Id.testId $ Id.List Id.showAllOptions
    , visibility case config.activeReasonCode of 
                    reasonCode -> if ( reasonCode == "OTHER" || reasonCode == "TECHNICAL_GLITCH") then VISIBLE else GONE
                    _               -> GONE
    ][  textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text config.showAllOptionsText
        , color Color.blue900
        , textSize FontSize.a_12
        , fontStyle $ FontStyle.semiBold LanguageStyle
        ]
    ]

 ]


cancelationReasonsList :: forall w .  (Action  -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
cancelationReasonsList push config = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , padding (PaddingVertical 0 24)
    , orientation VERTICAL
    ][ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ][cancelationReasonOptions config push],
          primaryButtons push config
    ]
    

cancelationReasonOptions :: forall w . Config -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
cancelationReasonOptions config push = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , padding (PaddingBottom 24)
  ] (mapWithIndex (\index item ->
      linearLayout 
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , orientation VERTICAL
      ][ linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , orientation VERTICAL 
          , padding (PaddingTop if index == 0 then 12 else 0)
          , visibility case config.activeReasonCode of 
                reasonCode -> if (( reasonCode == "TECHNICAL_GLITCH" && item.reasonCode /= "TECHNICAL_GLITCH") || ( reasonCode == "OTHER" && item.reasonCode /= "OTHER")) then GONE else VISIBLE
                _               -> VISIBLE
          , onClick (\action -> do
            _ <- push action 
            _ <- case item.reasonCode of 
              "OTHER" -> requestKeyboardShow (getNewIDWithTag "OtherReasonEditText")
              "TECHNICAL_GLITCH" -> requestKeyboardShow (getNewIDWithTag "TechGlitchEditText")
              _ -> pure unit 
            pure unit 
          ) (const (UpdateIndex index))
          , Id.testId $ Id.Option config.activeReasonCode
          ][radioButton config push index item,
            horizontalLine index (fromMaybe (-1) config.activeIndex) config,
            (case config.activeReasonCode of 
              reasonCode -> if (( reasonCode == "OTHER" && item.reasonCode == "OTHER")) then someOtherReason config push index else dummyTextView),
            (case config.activeReasonCode of 
              reasonCode -> if (( reasonCode == "TECHNICAL_GLITCH" && item.reasonCode == "TECHNICAL_GLITCH")) then technicalGlitchDescription config push index else dummyTextView)
            -- , technicalGlitchDescription config push index
           ]
          ]
      ) config.cancelRideReasons)


someOtherReason :: forall w . Config -> (Action  -> Effect Unit) -> Int -> PrestoDOM (Effect Unit) w
someOtherReason config push index = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  -- , margin (MarginLeft 40)
  , visibility case config.activeReasonCode of
                  reasonCode -> if (reasonCode == "OTHER") then VISIBLE else GONE 
                  _               -> GONE
  ][ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][  linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , stroke ("1," <> (if config.isLimitExceeded then Color.warningRed else Color.grey800))
          , gravity LEFT
          , cornerRadius 4.0
          , background Color.grey800
          , padding (Padding 16 2 16 2)
          ][  
            ((if os == "ANDROID" then editText else multiLineEditText)
              $ [ width MATCH_PARENT
              , height ( V 58)
              , color Color.black800
              , textSize FontSize.a_14
              , hint config.hint
              , hintColor Color.black650
              , fontStyle $ FontStyle.medium LanguageStyle
              , cornerRadius 4.0
              , background Color.grey800
              , singleLine false
              , onChange push (TextChanged ( getNewIDWithTag "OtherReasonEditText") )
              , Id.testId $ Id.TextField config.activeReasonCode
              , pattern "[A-Za-z0-9 ]*,100"
              ] <> (if os == "ANDROID" then [id (getNewIDWithTag "OtherReasonEditText")] else [] ))
            ]
          , textView
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text if (not config.isMandatoryTextHidden ) then config.strings.mandatory else config.strings.limitReached
            , color Color.warningRed
            , textSize FontSize.a_12
            , fontStyle $ FontStyle.regular LanguageStyle
            , visibility if ((not config.isMandatoryTextHidden )|| config.isLimitExceeded) then VISIBLE else GONE
            ]                              
          ]                                            
      ] 

technicalGlitchDescription :: forall w . Config -> (Action  -> Effect Unit) -> Int -> PrestoDOM (Effect Unit) w
technicalGlitchDescription config push index = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  -- , margin (MarginLeft 40)
  , visibility case config.activeReasonCode of
                  reasonCode -> if (reasonCode == "TECHNICAL_GLITCH" )  then VISIBLE else GONE 
                  _               -> GONE
  ][ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][  linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , stroke ("1," <> (if config.isLimitExceeded then Color.warningRed else Color.grey800))
          , gravity LEFT
          , cornerRadius 4.0
          , background Color.grey800
          , padding (Padding 16 2 16 2)
          ][  
            ((if os == "ANDROID" then editText else multiLineEditText)
              $ [ width MATCH_PARENT
              , height ( V 58)
              , color Color.black800
              , textSize FontSize.a_14
              , hint config.hint
              , hintColor Color.black650
              , background Color.grey800
              , fontStyle $ FontStyle.medium LanguageStyle
              , cornerRadius 4.0
              , singleLine false
              , onChange push (TextChanged ( getNewIDWithTag "TechGlitchEditText") )
              , Id.testId $ Id.TextField (Id.cancel <> Id.underScore <> Id.reason)
              , pattern "[A-Za-z0-9 ]*,100"
              ] <> (if os == "ANDROID" then [id (getNewIDWithTag "TechGlitchEditText")] else []))
            ]
          , textView
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text if (not config.isMandatoryTextHidden ) then config.strings.mandatory else config.strings.limitReached
            , color Color.warningRed
            , textSize FontSize.a_12
            , fontStyle $ FontStyle.regular LanguageStyle
            , visibility if ((not config.isMandatoryTextHidden )|| config.isLimitExceeded) then VISIBLE else GONE
            ]                              
          ]                                            
      ] 
primaryButtons :: forall w . (Action  -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
primaryButtons push config = 
 linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  ] [ PrimaryButton.view (push <<< Button1) (firstPrimaryButtonConfig config)
    , PrimaryButton.view (push <<< Button2) (secondPrimaryButtonConfig config)]


radioButton :: forall w .  Config -> (Action  -> Effect Unit) -> Int -> CancellationReasons -> PrestoDOM (Effect Unit) w
radioButton config push index item = 
 linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , padding (Padding 0 12 0 12)
  ][ imageView
      [ width (V 24)
      , height (V 24)
      , imageWithFallback case config.activeIndex of 
                    Just activeIndex' -> if ( index == activeIndex') then "ny_ic_radio_selected,https://assets.juspay.in/nammayatri/images/common/ny_ic_radio_selected.png" else "ny_ic_radio_unselected,https://assets.juspay.in/nammayatri/images/common/ny_ic_radio_unselected.png"
                    Nothing           -> "ny_ic_radio_unselected,https://assets.juspay.in/nammayatri/images/common/ny_ic_radio_unselected.png"
      ],
      textView
      [ text item.description
      , margin (MarginLeft 10)
      , fontStyle case config.activeIndex of 
                    Just activeIndex' -> if index == activeIndex' then FontStyle.bold LanguageStyle else FontStyle.regular LanguageStyle
                    Nothing           -> FontStyle.regular LanguageStyle
      , color Color.black900
      ]
  ]

firstPrimaryButtonConfig :: Config -> PrimaryButtonConfig.Config
firstPrimaryButtonConfig config = let
  config' = PrimaryButtonConfig.config
  primaryButtonConfig' =
    config'
      {textConfig
      { text = config.primaryButtonTextConfig.firstText
      , color = Color.black700}
      , background = Color.white900
      , stroke = "1," <> Color.black500
      , width = V ((screenWidth unit/2)-30)
      , id = "Button1"
      }
  in primaryButtonConfig'

secondPrimaryButtonConfig :: Config -> PrimaryButtonConfig.Config
secondPrimaryButtonConfig config = let
  config' = PrimaryButtonConfig.config
  primaryButtonConfig' =
    config'
       {textConfig
        { text = config.primaryButtonTextConfig.secondText}
        , width = V ((screenWidth unit/2)-30)
        , id = "Button2"
        , alpha = if(config.isCancelButtonActive) then 1.0  else 0.5
        , isClickable = config.isCancelButtonActive
       }
  in primaryButtonConfig'


horizontalLine :: forall w . Int -> Int -> Config -> PrestoDOM (Effect Unit) w
horizontalLine index activeIndex config = 
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , background Color.grey800
  , padding (PaddingVertical 2 2)
  , visibility if(((fromMaybe dummyReason( (config.cancelRideReasons)!!index)).reasonCode == "OTHER" && index == activeIndex) || (index == (length (config.cancelRideReasons) -1)) || ((fromMaybe dummyReason( (config.cancelRideReasons)!!index)).reasonCode == "TECHNICAL_GLITCH" && index == activeIndex)  ) then GONE else VISIBLE
  ][]

dummyTextView :: forall w . PrestoDOM (Effect Unit) w
dummyTextView = 
 textView
 [ width $ V 0
 , height $ V 0
 ]

dummyReason :: CancellationReasons
dummyReason = {
  reasonCode : ""
, description : ""
}