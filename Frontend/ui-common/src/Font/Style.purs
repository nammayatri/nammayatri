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

italic :: FontStyle
italic = fontByOS "PlusJakartaSans-Italic" "PlusJakartaSans-Italic" "Arial"

light :: LazyCheck -> FontStyle
light style = do
  case (getValueToLocalStore LANGUAGE_KEY) of
    "EN_US" -> fontByOS "PlusJakartaSans-Light" "PlusJakartaSans-Light" "Arial"
    "KN_IN" -> fontByOS "NotoSansKannada-Light" "NotoSansKannada-Light" "Arial"
    "HI_IN" -> fontByOS "PlusJakartaSans-Light" "PlusJakartaSans-Light" "Arial"
    _ -> fontByOS "PlusJakartaSans-Light" "PlusJakartaSans-Light" "Arial"

lightItalic :: FontStyle
lightItalic = fontByOS "PlusJakartaSans-LightItalic" "PlusJakartaSans-LightItalic" "Arial"

extraLight :: LazyCheck -> FontStyle
extraLight style = do
  case (getValueToLocalStore LANGUAGE_KEY) of
    "EN_US" -> fontByOS "PlusJakartaSans-ExtraLight" "PlusJakartaSans-ExtraLight" "Arial"
    "KN_IN" -> fontByOS "NotoSansKannada-ExtraLight" "NotoSansKannada-ExtraLight" "Arial"
    "HI_IN" -> fontByOS "PlusJakartaSans-ExtraLight" "PlusJakartaSans-ExtraLight" "Arial"
    _ -> fontByOS "PlusJakartaSans-ExtraLight" "PlusJakartaSans-ExtraLight" "Arial"

extraLightItalic :: FontStyle
extraLightItalic = fontByOS "PlusJakartaSans-ExtraLightItalic" "PlusJakartaSans-ExtraLightItalic" "Arial"

regular :: LazyCheck -> FontStyle
regular style = do
  case (getValueToLocalStore LANGUAGE_KEY) of
    "EN_US" -> fontByOS "PlusJakartaSans-Regular" "PlusJakartaSans-Regular" "Arial"
    "KN_IN" -> fontByOS "NotoSansKannada-Regular" "NotoSansKannada-Regular" "Arial"
    "HI_IN" -> fontByOS "PlusJakartaSans-Regular" "PlusJakartaSans-Regular" "Arial"
    _ -> fontByOS "PlusJakartaSans-Regular" "PlusJakartaSans-Regular" "Arial"

medium :: LazyCheck -> FontStyle
medium style = do
  case (getValueToLocalStore LANGUAGE_KEY) of
    "EN_US" -> fontByOS "PlusJakartaSans-Medium" "PlusJakartaSans-Medium" "Arial"
    "KN_IN" -> fontByOS "NotoSansKannada-Medium" "NotoSansKannada-Medium" "Arial"
    "HI_IN" -> fontByOS "PlusJakartaSans-Medium" "PlusJakartaSans-Medium" "Arial"
    _ -> fontByOS "PlusJakartaSans-Medium" "PlusJakartaSans-Medium" "Arial"

mediumItalic :: FontStyle
mediumItalic = fontByOS "PlusJakartaSans-MediumItalic" "PlusJakartaSans-MediumItalic" "Arial"

semiBold :: LazyCheck -> FontStyle
semiBold style = do
  case (getValueToLocalStore LANGUAGE_KEY) of
    "EN_US" -> fontByOS "PlusJakartaSans-SemiBold" "PlusJakartaSans-SemiBold" "Arial"
    "KN_IN" -> fontByOS "NotoSansKannada-SemiBold" "NotoSansKannada-SemiBold" "Arial"
    "HI_IN" -> fontByOS "PlusJakartaSans-SemiBold" "PlusJakartaSans-SemiBold" "Arial"
    _ -> fontByOS "PlusJakartaSans-SemiBold" "PlusJakartaSans-SemiBold" "Arial"

semiBoldItalic :: FontStyle
semiBoldItalic = fontByOS "PlusJakartaSans-SemiBoldItalic" "PlusJakartaSans-SemiBoldItalic" "Arial" 

bold :: LazyCheck -> FontStyle
bold style = do
  case (getValueToLocalStore LANGUAGE_KEY) of
    "EN_US" -> fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"
    "KN_IN" -> fontByOS "NotoSansKannada-Bold" "NotoSansKannada-Bold" "Arial"
    "HI_IN" -> fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"
    _ -> fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"

boldItalic :: FontStyle
boldItalic = fontByOS "PlusJakartaSans-BoldItalic" "PlusJakartaSans-BoldItalic" "Arial"

extraBold :: LazyCheck -> FontStyle
extraBold style = do
  case (getValueToLocalStore LANGUAGE_KEY) of
    "EN_US" -> fontByOS "PlusJakartaSans-ExtraBold" "PlusJakartaSans-ExtraBold" "Arial"
    "KN_IN" -> fontByOS "NotoSansKannada-ExtraBold" "NotoSansKannada-ExtraBold" "Arial"
    "HI_IN" -> fontByOS "PlusJakartaSans-ExtraBold" "PlusJakartaSans-ExtraBold" "Arial"
    _ -> fontByOS "PlusJakartaSans-ExtraBold" "PlusJakartaSans-ExtraBold" "Arial"

extraBoldItalic :: FontStyle
extraBoldItalic = fontByOS "PlusJakartaSans-ExtraBoldItalic" "PlusJakartaSans-ExtraBoldItalic" "Arial"

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
      "IOS" -> ios
      "WEB" -> web
      _ -> android
