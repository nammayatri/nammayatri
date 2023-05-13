{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Font.Style where

import Font.Size as FontSize
import Styles.Types (FontStyle)
import Halogen.VDom.DOM.Prop (Prop)
import Engineering.Helpers.Commons (os)
import PrestoDOM (fontStyle, lineHeight, textSize)
import Prelude ((==),($),(/=))
import Storage (getValueToLocalStore, KeyStore(..))
import Common.Types.App
import Debug

italic :: FontStyle
italic = fontByOS "Roboto-Italic" "Roboto-Italic" "Arial"

light :: LazyCheck -> FontStyle
light style = do
  case (getValueToLocalStore LANGUAGE_KEY) of
    "EN_US" -> fontByOS "Roboto-Light" "Roboto-Light" "Arial"
    "KN_IN" -> fontByOS "Roboto-Light" "Roboto-Light" "Arial"
    "HI_IN" -> fontByOS "Roboto-Light" "Roboto-Light" "Arial"
    _ -> fontByOS "Roboto-Light" "Roboto-Light" "Arial"

lightItalic :: FontStyle
lightItalic = fontByOS "Roboto-LightItalic" "Roboto-LightItalic" "Arial"

extraLight :: LazyCheck -> FontStyle
extraLight style = do
  case (getValueToLocalStore LANGUAGE_KEY) of
    "EN_US" -> fontByOS "Roboto-ExtraLight" "Roboto-ExtraLight" "Arial"
    "KN_IN" -> fontByOS "Roboto-ExtraLight" "Roboto-ExtraLight" "Arial"
    "HI_IN" -> fontByOS "Roboto-ExtraLight" "Roboto-ExtraLight" "Arial"
    _ -> fontByOS "Roboto-ExtraLight" "Roboto-ExtraLight" "Arial"

extraLightItalic :: FontStyle
extraLightItalic = fontByOS "Roboto-ExtraLightItalic" "Roboto-ExtraLightItalic" "Arial"

regular :: LazyCheck -> FontStyle
regular style = do
  case (getValueToLocalStore LANGUAGE_KEY) of
    "EN_US" -> fontByOS "Roboto-Regular" "Roboto-Regular" "Arial"
    "KN_IN" -> fontByOS "Roboto-Regular" "Roboto-Regular" "Arial"
    "HI_IN" -> fontByOS "Roboto-Regular" "Roboto-Regular" "Arial"
    _ -> fontByOS "Roboto-Regular" "Roboto-Regular" "Arial"

medium :: LazyCheck -> FontStyle
medium style = do
  case (getValueToLocalStore LANGUAGE_KEY) of
    "EN_US" -> fontByOS "Roboto-Medium" "Roboto-Medium" "Arial"
    "KN_IN" -> fontByOS "Roboto-Medium" "Roboto-Medium" "Arial"
    "HI_IN" -> fontByOS "Roboto-Medium" "Roboto-Medium" "Arial"
    _ -> fontByOS "Roboto-Medium" "Roboto-Medium" "Arial"

mediumItalic :: FontStyle
mediumItalic = fontByOS "Roboto-MediumItalic" "Roboto-MediumItalic" "Arial"

semiBold :: LazyCheck -> FontStyle
semiBold style = do
  case (getValueToLocalStore LANGUAGE_KEY) of
    "EN_US" -> fontByOS "Roboto-Bold" "Roboto-Bold" "Arial"
    "KN_IN" -> fontByOS "Roboto-Bold" "Roboto-Bold" "Arial"
    "HI_IN" -> fontByOS "Roboto-Bold" "Roboto-Bold" "Arial"
    _ -> fontByOS "Roboto-Bold" "Roboto-Bold" "Arial"

semiBoldItalic :: FontStyle
semiBoldItalic = fontByOS "Roboto-SemiBoldItalic" "Roboto-SemiBoldItalic" "Arial" 

bold :: LazyCheck -> FontStyle
bold style = do
  case (getValueToLocalStore LANGUAGE_KEY) of
    "EN_US" -> fontByOS "Roboto-Bold" "Roboto-Bold" "Arial"
    "KN_IN" -> fontByOS "Roboto-Bold" "Roboto-Bold" "Arial"
    "HI_IN" -> fontByOS "Roboto-Bold" "Roboto-Bold" "Arial"
    _ -> fontByOS "Roboto-Bold" "Roboto-Bold" "Arial"

boldItalic :: FontStyle
boldItalic = fontByOS "Roboto-BoldItalic" "Roboto-BoldItalic" "Arial"

extraBold :: LazyCheck -> FontStyle
extraBold style = do
  case (getValueToLocalStore LANGUAGE_KEY) of
    "EN_US" -> fontByOS "Roboto-ExtraBold" "Roboto-ExtraBold" "Arial"
    "KN_IN" -> fontByOS "Roboto-ExtraBold" "Roboto-ExtraBold" "Arial"
    "HI_IN" -> fontByOS "Roboto-ExtraBold" "Roboto-ExtraBold" "Arial"
    _ -> fontByOS "Roboto-ExtraBold" "Roboto-ExtraBold" "Arial"

extraBoldItalic :: FontStyle
extraBoldItalic = fontByOS "Roboto-ExtraBoldItalic" "Roboto-ExtraBoldItalic" "Arial"

h1 :: LazyCheck -> forall properties. (Array (Prop properties))
h1 typography = [
  fontStyle $ bold LanguageStyle
, textSize FontSize.a_22
, lineHeight "28"
]

h2 :: LazyCheck -> forall properties. (Array (Prop properties))
h2 typography = [
  fontStyle $ bold LanguageStyle
, textSize FontSize.a_18
, lineHeight "23"
]

h3 :: LazyCheck ->  forall properties. (Array (Prop properties))
h3 typography = [
  fontStyle $ semiBold LanguageStyle
, textSize FontSize.a_18
, lineHeight "23"
]

subHeading1 :: LazyCheck -> forall properties. (Array (Prop properties))
subHeading1 typography = [
  fontStyle $ semiBold LanguageStyle
, textSize FontSize.a_16
, lineHeight "24"
]

subHeading2 :: LazyCheck ->  forall properties. (Array (Prop properties))
subHeading2 typography = [
  fontStyle $ medium LanguageStyle
, textSize FontSize.a_16
, lineHeight "24"
]

body1 ::  LazyCheck -> forall properties. (Array (Prop properties))
body1 typography = [
  fontStyle $ medium LanguageStyle
, textSize FontSize.a_14
, lineHeight "18"
] 

body2 :: LazyCheck -> forall properties. (Array (Prop properties))
body2 typography = [
  fontStyle mediumItalic
, textSize FontSize.a_14
, lineHeight "20"
]

body3 ::  LazyCheck -> forall properties. (Array (Prop properties))
body3 typography = [
  fontStyle $ regular LanguageStyle
, textSize FontSize.a_12
, lineHeight "16"
]

paragraphText :: LazyCheck -> forall properties. (Array (Prop properties))
paragraphText typography = [
  fontStyle $ regular LanguageStyle
, textSize FontSize.a_14
, lineHeight "18"
]

tags ::  LazyCheck -> forall properties. (Array (Prop properties))
tags typography = [
  fontStyle $ medium LanguageStyle
, textSize FontSize.a_12
, lineHeight "15"
]

captions ::  LazyCheck ->  forall properties. (Array (Prop properties))
captions typography = [
  fontStyle $ regular LanguageStyle
, textSize FontSize.a_10
, lineHeight "13"
]

priceFont ::  LazyCheck -> forall properties. (Array (Prop properties))
priceFont typography = [
    fontStyle $ bold LanguageStyle
  , textSize FontSize.a_32
  , lineHeight "40"
]

priceFont_big :: LazyCheck -> forall properties. (Array (Prop properties))
priceFont_big typography = [
    fontStyle $ bold LanguageStyle
  , textSize FontSize.a_44
  , lineHeight "40"
]

fontByOS :: forall a. a -> a -> a -> a
fontByOS android ios web
  = case os of
      "IOS" -> spy "font by ios" ios
      "WEB" -> spy "font by web" web
      _ -> spy "font by android" android
