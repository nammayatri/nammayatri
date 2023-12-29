{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module ReactComponents.PrimaryEditText.Views where

import Prelude (Unit, pure, ($), (<>))
import Effect (Effect)
-- import PrestoDOM (Gravity(..), InputType(..), Length(..), Margin(..), Orientation(..) "0, 10, 20, 10"
import ReactComponents.PrimaryEditText.Controllers (ComponentAction(..), Action(..))
import Font.Style as FontStyle
import Styles.Colors as Color
import Font.Size as FontSize
import Screens.Types (PrimaryEditTextState)
import Data.Maybe (fromMaybe)
import PrestoDOM.Animation as PrestoAnim
import Animation (fadeIn, fadeOut)
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Common.Types.App

import React.Render.CustomBase (editText, linearLayout, textView)
import React.Basic.Hooks as React
import React.Basic.Hooks (Component, JSX, component, useState, (/\))

app :: (ComponentAction -> Effect Unit) -> Component PrimaryEditTextState
app push = do
  component "PrimaryEditText" \initialState -> React.do
    state /\ setState <- useState initialState
    pure $ view push setState state

view :: (ComponentAction -> Effect Unit) -> ((PrimaryEditTextState -> PrimaryEditTextState) -> Effect Unit) -> PrimaryEditTextState -> JSX
view push setState state =
  linearLayout
    { width: "match_parent"
    , height: "wrap_content"
    , orientation: "vertical"
    }
    [ linearLayout
        { width: "match_parent"
        , height: "wrap_content"
        , orientation: "horizontal"
        }
        [ textView $
            { width: "wrap_content"
            , height: "wrap_content"
            , text: state.title
            , color: Color.greyTextColor
            , alpha: "0.8"
            -- , margin: (MarginBottom 10)
            -- , visibility: (if state.title == (getString MOBILE_NUMBER) then GONE else VISIBLE)
            }
        , textView $
            { width: "wrap_content"
            , height: "wrap_content"
            , text: "  *"
            , color: Color.redRoman
            , alpha: "0.8"
            -- , margin: (MarginBottom 10)
            -- , visibility: (if state.title == (getString MOBILE_NUMBER) || state.valueId == "EditTextOtp" then GONE else VISIBLE)
            }
        ]
    , linearLayout
        { width: "match_parent"
        , height: "wrap_content"
        , orientation: "horizontal"
        , stroke: (if state.isinValid then ("1," <> Color.lightMaroon) else ("1," <> Color.borderColorLight))
        , cornerRadius: "4.0"
        }
        [ textView
            ( { width: "60"
              , height: "match_parent"
              , text: (getString COUNTRY_CODE_INDIA)
              , background: Color.greyBackground
              , gravity: "center"
              -- , cornerRadii: (Corners 4.0 true false false true)
              -- , visibility: GONE
              }
            )
        , textView
            { width: "20"
            , height: "wrap_content"
            }
        , editText
            ( { width: "match_parent"
              , height: "54"
              , padding: "0, 10, 20, 10"
              , color: Color.greyTextColor
              , text: state.text
              -- , hint: state.hint
              -- , inputType: (if state.type == "number" then Numeric else if state.type == "password" then Password else TypeText)
              -- , inputTypeI: (if state.valueId == "VEHICLE_NUMBER" || state.valueId == "VEHICLE_PERMIT_NUMBER" then 4097 else if state.valueId == "DRIVER_NAME" then 1 else if state.valueId == "MODEL_TYPE" || state.valueId == "COLOR" then 1 else 2)
              , weight: "1.0"
              , cornerRadius: "4.0"
              -- , pattern: (fromMaybe "[a-z, 0-9, A-Z]" state.pattern)
              -- , letterSpacing: state.letterSpacing
              -- , onChange: push (TextChanged state.valueId)
              , stroke: (if state.isinValid then ("1," <> Color.white900) else ("1," <> Color.white900))
              -- , onFocus: push (const TextClicked)
              , id: state.id
              }
            )
        ]
    -- , PrestoAnim.animationSet [ fadeIn state.isinValid, fadeOut $ not state.isinValid ]
    , textView
        { width: "wrap_content"
        , height: "wrap_content"
        , text: (fromMaybe "" state.error)
        -- , visibility: (if state.isinValid then VISIBLE else GONE)
        , color: Color.lightMaroon
        -- , margin: (MarginTop 10)
        }
    ]