{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module ReactComponents.InAppKeyboardModal.View where

import Common.Types.App
import Prelude (Unit, map, not, pure, show, ($), (&&), (<>), (==), (||), (/), unit, (>=))
import React.Basic.Hooks (Component, JSX, component, useState, (/\), useEffect, mkReducer, useReducer)
import React.Render.CustomBase (imageView, linearLayout, textView)

import Animation (translateYAnim)
import Animation.Config (translateYAnimConfig)
import Data.Array (mapWithIndex)
import Data.Array as DA
import Data.String (drop, take)
import Debug (spy)
import Effect (Effect)
import Engineering.Helpers.Commons (screenWidth)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM.Animation as PrestoAnim
-- import PrestoDOM.Properties (background, backgroundDrawable, clickable, color, cornerRadii, cornerRadius, fontStyle, gravity, height, imageUrl, margin, orientation, padding, stroke, text, textSize, weight, width, visibility, imageWithFallback)
import PrestoDOM.Types.DomAttributes (Corners(..), Visibility(..))
import React.Basic.Hooks as React
import ReactComponents.InAppKeyboardModal.Controller (ComponentAction(..), InAppKeyboardModalState, ComponentOutput(..), reducerFn)
import Screens.Types (KeyboardModalType(..)) as KeyboardModalType
import Styles.Colors as Color
import Debug (spy)
import Data.String (length)
import Effect.Unsafe (unsafePerformEffect)
-- import Effect.Console (log)

app :: (ComponentOutput -> Effect Unit) -> Component InAppKeyboardModalState
app push = do
  component "InAppKeyboardModal" \initialState -> React.do
    let reducer = unsafePerformEffect $ mkReducer reducerFn
    state /\ dispatch <- useReducer initialState reducer
    useEffect state.inputTextConfig.text
      ( \_ -> do
          let _ = spy "InAppKeyboardModal useEffect" state
          if(length state.inputTextConfig.text >= 4) 
            then pure $ push $ OnClickDone state.inputTextConfig.text
            else pure $ pure unit
      )
    pure $ view push dispatch state

view :: (ComponentOutput -> Effect Unit) -> (ComponentAction -> Effect Unit) -> InAppKeyboardModalState -> JSX
view push componentPush state =
  linearLayout
    { width: "match_parent"
    , height: "match_parent"
    , orientation: "vertical"
    , clickable: "true"
    , background: Color.black9000
    , gravity: "bottom"
    }
    [
      -- PrestoAnim.animationSet
      --   { translateYAnim: translateYAnimConfig
      --   }
      linearLayout
        { width: "match_parent"
        , height: "wrap_content"
        , cornerRadius: "20.0"
        , orientation: "vertical"
        , background: Color.white900
        , gravity: "center"
        }
        [ linearLayout
            { width: "match_parent"
            , height: "wrap_content"
            , orientation: "vertical"
            , margin: "0, 10, 0, 0"
            , gravity: "center"
            }
            [ linearLayout
                { width: "match_parent"
                , height: "wrap_content"
                , orientation: "vertical"
                , gravity: "center"
                , margin: "20 20 20 20"
                }
                [ imageView
                    { width: "35"
                    , height: "35"
                    , imageWithFallback: fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
                    , onClick: push BackPressed
                    -- , onClick: push (const BackPressed)
                    , padding: "5, 5, 5, 5"
                    }
                -- , textView {text : "Working"}
                , textView
                    { width: show state.headingConfig.width
                    , height: show state.headingConfig.height
                    , gravity: state.headingConfig.gravity
                    , text: state.headingConfig.text
                    , color: state.headingConfig.color
                    , margin: state.headingConfig.margin
                    , visibility: state.headingConfig.visibility
                    -- , cornerRadius: state.headingConfig.cornerRadius
                    , padding: state.headingConfig.padding
                    , weight: show state.headingConfig.weight
                    -- , fontStyle: FontStyle.getFontStyle state.inputTextConfig.textStyle LanguageStyle
                    }
                ]
            , otpView push componentPush state
            ]
        , keyboard push componentPush state
        ]
    ]

textBoxes :: (ComponentOutput -> Effect Unit) -> (ComponentAction -> Effect Unit) -> InAppKeyboardModalState -> JSX
textBoxes push componentPush state =
  linearLayout
    { width: "match_parent"
    , height: "wrap_content"
    , orientation: "horizontal"
    , gravity: "center"
    , visibility: if state.modalType == KeyboardModalType.OTP && not state.otpAttemptsExceeded then "visible" else "gone"
    , margin: "0, 20, 0, 20"
    , clickable: "false"
    }
    ( mapWithIndex
        ( \index item ->
            textView
              { width: show state.textBoxConfig.width
              , height: show state.textBoxConfig.height
              , color: Color.greyTextColor
              , text: take 1 (drop index state.inputTextConfig.text)
              , gravity: "center"
              , cornerRadius: "4.0"
              , stroke: "1," <> if (state.otpIncorrect || state.otpAttemptsExceeded) then Color.textDanger else if state.inputTextConfig.focusIndex == index then Color.highlightBorderColor else Color.borderColorLight
              -- , margin: (show (screenWidth unit)/30) <> ", 0 " <> (show (screenWidth unit)/30) <> ", 0"
              -- , onClick: push (const (OnclickTextBox index))
              -- , fontStyle: FontStyle.getFontStyle state.inputTextConfig.textStyle LanguageStyle
              }
        )
        state.textBoxConfig.textBoxesArray
    )

singleTextBox :: (ComponentOutput -> Effect Unit) -> (ComponentAction -> Effect Unit) -> InAppKeyboardModalState -> JSX
singleTextBox push componentPush state =
  linearLayout
    { width: "match_parent"
    , height: "wrap_content"
    , orientation: "horizontal"
    , gravity: "center"
    , cornerRadius: "4.0"
    , visibility: if state.modalType == KeyboardModalType.MOBILE__NUMBER then "visible" else "gone"
    , clickable: "false"
    , padding: "16, 16, 16, 16"
    , stroke: "1," <> if not state.isValidAlternateNumber then Color.textDanger else Color.borderColorLight
    }
    [ textView
        { width: show state.inputTextConfig.width
        , height: show state.inputTextConfig.height
        , color: state.inputTextConfig.color
        , text: state.inputTextConfig.text
        , weight: show state.inputTextConfig.weight
        , gravity: state.inputTextConfig.gravity
        , visibility: state.inputTextConfig.visibility
        -- , cornerRadius: state.inputTextConfig.cornerRadius
        , padding: state.inputTextConfig.padding
        , margin: state.inputTextConfig.margin
        -- , onClick: push (const (OnclickTextBox 0))
        -- , fontStyle: FontStyle.getFontStyle state.inputTextConfig.textStyle LanguageStyle
        }
    , imageView
        { width: "23"
        , height: "23"
        , imageWithFallback: fetchImage FF_COMMON_ASSET "ny_ic_close"
        , visibility: if (state.inputTextConfig.text == (getString ENTER_MOBILE_NUMBER)) then "gone" else "visible"
        -- , onClick: push (const (OnClickTextCross))
        , onClick: componentPush OnClickTextCross
        }
    ]

otpView :: (ComponentOutput -> Effect Unit) -> (ComponentAction -> Effect Unit) -> InAppKeyboardModalState -> JSX
otpView push componentPush state =
  linearLayout
    { width: "match_parent"
    , height: "wrap_content"
    , margin: "20, 0, 20, 0"
    , orientation: "vertical"
    , gravity: if (state.modalType == KeyboardModalType.OTP) then "center" else "left"
    }
    [ textBoxes push componentPush state
    , singleTextBox push componentPush state
    , textView
        { width: show state.subHeadingConfig.width
        , height: show state.subHeadingConfig.height
        , color: state.subHeadingConfig.color
        , text: state.subHeadingConfig.text
        , visibility: state.subHeadingConfig.visibility
        , gravity: state.subHeadingConfig.gravity
        -- , cornerRadius: state.subHeadingConfig.cornerRadius
        , padding: state.subHeadingConfig.padding
        , margin: state.subHeadingConfig.margin
        , weight: show state.subHeadingConfig.weight
        -- , fontStyle: FontStyle.getFontStyle state.subHeadingConfig.textStyle LanguageStyle
        }
    , textView
        { width: show state.errorConfig.width
        , height: show state.errorConfig.width
        , visibility: state.errorConfig.visibility
        , margin: state.errorConfig.margin
        , text: state.errorConfig.text
        , color: state.errorConfig.color
        , gravity: state.errorConfig.gravity
        -- , cornerRadius: state.errorConfig.cornerRadius
        , padding: state.errorConfig.padding
        , weight: show state.errorConfig.weight
        -- , fontStyle: FontStyle.getFontStyle state.errorConfig.textStyle LanguageStyle
        }
    , textView
        { width: "wrap_content"
        , height: "wrap_content"
        , text: getString RESEND_OTP
        , color: Color.blue900
        , margin: "0, 0, 0, 0"
        -- , onClick: push (const (OnClickResendOtp))
        , visibility: if (state.modalType == KeyboardModalType.OTP && state.showResendOtpButton && (not state.otpAttemptsExceeded)) then "visible" else "gone"
        -- , fontStyle: FontStyle.tags TypoGraphy
        }
    ]

keyboard :: (ComponentOutput -> Effect Unit) -> (ComponentAction -> Effect Unit) -> InAppKeyboardModalState -> JSX
keyboard push componentPush state =
  linearLayout
    { width: "match_parent"
    , height: "wrap_content"
    , orientation: "vertical"
    , margin: "0, 40, 0, 0"
    , padding: "0, 5, 0, 20"
    , gravity: "center"
    , background: Color.grey800
    }
    ( map
        ( \(item) ->
            linearLayout
              { width: "match_parent"
              , height: "wrap_content"
              , orientation: "horizontal"
              , margin: "4, 0, 4, 0"
              , gravity: "center"
              }
              ( mapWithIndex
                  ( \index key ->
                      linearLayout
                        { width: "match_parent"
                        , height: "wrap_content"
                        , gravity: "center"
                        , weight: "1.0"
                        -- , backgroundDrawable: "button"
                        }
                        [ if (key == "back" || key == "done") then
                            linearLayout
                              { width: "match_parent"
                              , height: "wrap_content"
                              , gravity: "center"
                              , margin: "3, 3, 3, 3"
                              , alpha: show $ if (key == "done") then state.imageConfig.alpha else 1.0
                              , background: if key == "back" then Color.lightGrey else Color.darkMint
                              , cornerRadius: "4.0"
                              -- , cornerRadii: if key == "back" then Corners 30.0 false false false true else Corners 30.0 false false true false
                              -- , onClick: push if key == "back" then (const (OnClickBack state.inputTextConfig.text)) else (const (OnClickDone state.inputTextConfig.text))
                              , onClick: componentPush $ OnClickBack state.inputTextConfig.text
                              -- , clickable: if key == "back" then "true" else if (length state.inputTextConfig.text == (DA.length state.textBoxConfig.textBoxesArray) && state.modalType == KeyboardModalType.OTP && not state.otpIncorrect) || (length state.inputTextConfig.text == 10 && state.modalType == KeyboardModalType.MOBILE__NUMBER && state.isValidAlternateNumber) then "true" else "false"
                              }
                              [ if key == "back" then
                                  imageView
                                    { width: "24"
                                    , height: "24"
                                    , imageWithFallback: fetchImage FF_COMMON_ASSET "ny_ic_delete"
                                    , margin: "0, 18, 0, 18"
                                    }
                                else
                                  imageView
                                    { width: "24"
                                    , height: "24"
                                    , imageWithFallback: fetchImage FF_COMMON_ASSET "ny_ic_tick_white"
                                    , margin: "0, 18, 0, 18"
                                    }
                              ]
                          else
                            linearLayout
                              { width: "match_parent"
                              , height: "wrap_content"
                              , gravity: "center"
                              , margin: "3, 3, 3, 3"
                              , background: Color.white900
                              , cornerRadius: "4.0"
                              , onClick: if(length state.inputTextConfig.text == 3) 
                                            then push (OnClickDone (state.inputTextConfig.text <> key)) 
                                            else componentPush $ OnSelection key state.inputTextConfig.focusIndex
                              -- , onClick: push (const (OnSelection key state.inputTextConfig.focusIndex))
                              }
                              [ textView
                                  { width: "wrap_content"
                                  , height: "match_parent"
                                  , text: key
                                  , color: Color.greyTextColor
                                  , padding: "0, 15, 0, 15"
                                  -- , fontStyle: FontStyle.h1 TypoGraphy
                                  }
                              ]
                        ]
                  )
                  item.keys
              )
        )
        state.keyList
    )