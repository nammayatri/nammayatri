{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Font.Style where


import Common.Types.App
import Debug

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Engineering.Helpers.Commons (os)
import Font.Size as FontSize
import Foreign.Generic (class Decode, class Encode)
import Foreign.Generic.EnumEncoding (decodeEnum)
import Halogen.VDom.DOM.Prop (Prop)
import Prelude (class Eq, class Show, ($), (/=), (==), (<>), (<<<))
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)
import PrestoDOM (FontWeight(..), fontStyle, lineHeight, textSize, fontWeight)
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Types (FontStyle, FontType(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), hush)
import Control.Monad.Except (runExcept)
import Foreign.Generic (Foreign, decode)

foreign import getFontStyleFromConfig :: String -> Foreign

getLanguageFromLocalStore :: Unit -> String
getLanguageFromLocalStore _ = JBridge.getKeyInSharedPrefKeys "LANGUAGE_KEY"

getFontType :: String ->  FontType
getFontType dummy = case spy "get Font Type "(decodeFont (getFontStyleFromConfig "")) of
  Just font -> font
  Nothing -> Assets

decodeFont :: Foreign -> Maybe FontType
decodeFont = hush <<< runExcept <<< decode

italic :: FontStyle
italic = fontByOS "Roboto-Italic" "Roboto-Italic" "Arial"

light :: LazyCheck -> FontStyle
light style = do
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS "PlusJakartaSans-Light" "PlusJakartaSans-Light" "Arial"
    "KN_IN" -> fontByOS "NotoSansKannada-Light" "NotoSansKannada-Light" "Arial"
    "HI_IN" -> fontByOS "PlusJakartaSans-Light" "PlusJakartaSans-Light" "Arial"
    _ -> fontByOS "PlusJakartaSans-Light" "PlusJakartaSans-Light" "Arial"

lightItalic :: FontStyle
lightItalic = fontByOS "Roboto-LightItalic" "Roboto-LightItalic" "Arial"

extraLight :: LazyCheck -> FontStyle
extraLight style = do
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS "PlusJakartaSans-ExtraLight" "PlusJakartaSans-ExtraLight" "Arial"
    "KN_IN" -> fontByOS "NotoSansKannada-ExtraLight" "NotoSansKannada-ExtraLight" "Arial"
    "HI_IN" -> fontByOS "PlusJakartaSans-ExtraLight" "PlusJakartaSans-ExtraLight" "Arial"
    _ -> fontByOS "PlusJakartaSans-ExtraLight" "PlusJakartaSans-ExtraLight" "Arial"

extraLightItalic :: FontStyle
extraLightItalic = fontByOS "Roboto-ExtraLightItalic" "Roboto-ExtraLightItalic" "Arial"

regular :: LazyCheck -> FontStyle
regular style = do
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS "PlusJakartaSans-Regular" "PlusJakartaSans-Regular" "Arial"
    "KN_IN" -> fontByOS "NotoSansKannada-Regular" "NotoSansKannada-Regular" "Arial"
    "HI_IN" -> fontByOS "PlusJakartaSans-Regular" "PlusJakartaSans-Regular" "Arial"
    _ -> fontByOS "PlusJakartaSans-Regular" "PlusJakartaSans-Regular" "Arial"

medium :: LazyCheck -> FontStyle
medium style = do
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS "PlusJakartaSans-Medium" "PlusJakartaSans-Medium" "Arial"
    "KN_IN" -> fontByOS "NotoSansKannada-Medium" "NotoSansKannada-Medium" "Arial"
    "HI_IN" -> fontByOS "PlusJakartaSans-Medium" "PlusJakartaSans-Medium" "Arial"
    _ -> fontByOS "PlusJakartaSans-Medium" "PlusJakartaSans-Medium" "Arial"

mediumItalic :: FontStyle
mediumItalic = fontByOS "Roboto-MediumItalic" "Roboto-MediumItalic" "Arial"

semiBold :: LazyCheck -> FontStyle
semiBold style = do
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS "PlusJakartaSans-SemiBold" "PlusJakartaSans-SemiBold" "Arial"
    "KN_IN" -> fontByOS "NotoSansKannada-SemiBold" "NotoSansKannada-SemiBold" "Arial"
    "HI_IN" -> fontByOS "PlusJakartaSans-SemiBold" "PlusJakartaSans-SemiBold" "Arial"
    _ -> fontByOS "PlusJakartaSans-SemiBold" "PlusJakartaSans-SemiBold" "Arial"

semiBoldItalic :: FontStyle
semiBoldItalic = fontByOS "PlusJakartaSans-SemiBoldItalic" "PlusJakartaSans-SemiBoldItalic" "Arial"

bold :: LazyCheck -> FontStyle
bold style = do
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"
    "KN_IN" -> fontByOS "NotoSansKannada-Bold" "NotoSansKannada-Bold" "Arial"
    "HI_IN" -> fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"
    _ -> fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"

boldItalic :: FontStyle
boldItalic = fontByOS "Roboto-BoldItalic" "Roboto-BoldItalic" "Arial"

extraBold :: LazyCheck -> FontStyle
extraBold style = do
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS "PlusJakartaSans-ExtraBold" "PlusJakartaSans-ExtraBold" "Arial"
    "KN_IN" -> fontByOS "NotoSansKannada-ExtraBold" "NotoSansKannada-ExtraBold" "Arial"
    "HI_IN" -> fontByOS "PlusJakartaSans-ExtraBold" "PlusJakartaSans-ExtraBold" "Arial"
    _ -> fontByOS "PlusJakartaSans-ExtraBold" "PlusJakartaSans-ExtraBold" "Arial"

extraBoldItalic :: FontStyle
extraBoldItalic = fontByOS "Roboto-ExtraBoldItalic" "Roboto-ExtraBoldItalic" "Arial"

h1 :: LazyCheck -> forall properties. (Array (Prop properties))
h1 typography = [
  textSize FontSize.a_22
, lineHeight "28"
] <> if (getFontType "") == Assets then [fontStyle $ bold LanguageStyle] else [fontWeight $ FontWeight 700]

h2 :: LazyCheck -> forall properties. (Array (Prop properties))
h2 typography = [
  textSize FontSize.a_18
, lineHeight "23"
] <> if (getFontType "") == Assets then [fontStyle $ bold LanguageStyle] else [fontWeight $ FontWeight 700]

h3 :: LazyCheck ->  forall properties. (Array (Prop properties))
h3 typography = [
   textSize FontSize.a_18
, lineHeight "23"
] <> if (getFontType "") == Assets then [fontStyle $ semiBold LanguageStyle] else [fontWeight $ FontWeight 600]

subHeading1 :: LazyCheck -> forall properties. (Array (Prop properties))
subHeading1 typography = [
  textSize FontSize.a_16
, lineHeight "24"
] <> if (getFontType "")  == Assets then [fontStyle $ semiBold LanguageStyle] else [fontWeight $ FontWeight 600]

subHeading2 :: LazyCheck ->  forall properties. (Array (Prop properties))
subHeading2 typography = [
  textSize FontSize.a_16
, lineHeight "24"
] <> if (getFontType "") == Assets then [fontStyle $ medium LanguageStyle] else [fontWeight $ FontWeight 500]

body1 ::  LazyCheck -> forall properties. (Array (Prop properties))
body1 typography = [
  textSize FontSize.a_14
, lineHeight "18"
] <> if (getFontType "") == Assets then [fontStyle $ medium LanguageStyle] else [fontWeight $ FontWeight 500]

body2 :: LazyCheck -> forall properties. (Array (Prop properties))
body2 typography = [
 textSize FontSize.a_14
, lineHeight "20"
] <> if (getFontType "") == Assets then [fontStyle $ medium LanguageStyle] else [fontWeight $ FontWeightWithItalic 500 true]

body3 ::  LazyCheck -> forall properties. (Array (Prop properties))
body3 typography = [
  textSize FontSize.a_12
, lineHeight "16"
] <> if (getFontType "") == Assets then [fontStyle $ regular LanguageStyle] else [fontWeight $ FontWeight 400]

paragraphText :: LazyCheck -> forall properties. (Array (Prop properties))
paragraphText typography = [
  textSize FontSize.a_14
, lineHeight "18"
]  <> if (getFontType "") == Assets then [fontStyle $ regular LanguageStyle] else [fontWeight $ FontWeight 400]

tags ::  LazyCheck -> forall properties. (Array (Prop properties))
tags typography = [
  textSize FontSize.a_12
, lineHeight "15"
]  <> if (getFontType "") == Assets then [fontStyle $ medium LanguageStyle] else [fontWeight $ FontWeight 500]

captions ::  LazyCheck ->  forall properties. (Array (Prop properties))
captions typography = [
  textSize FontSize.a_10
, lineHeight "13"
]  <> if (getFontType "") == Assets then [fontStyle $ regular LanguageStyle] else [fontWeight $ FontWeight 400]

priceFont ::  LazyCheck -> forall properties. (Array (Prop properties))
priceFont typography = [
    textSize FontSize.a_40
  , lineHeight "40"
]  <> if (getFontType "") == Assets then [fontStyle $ bold LanguageStyle] else [fontWeight $ FontWeight 700]

priceFont_big :: LazyCheck -> forall properties. (Array (Prop properties))
priceFont_big typography = [
    textSize FontSize.a_44
  , lineHeight "40"
]  <> if (getFontType "") == Assets then [fontStyle $ bold LanguageStyle] else [fontWeight $ FontWeight 700]

fontByOS :: forall a. a -> a -> a -> a
fontByOS android ios web
  = case os of
      "IOS" -> spy "font by ios" ios
      "WEB" -> spy "font by web" web
      _ -> spy "font by android" android

body4 ::  LazyCheck -> forall properties. (Array (Prop properties))
body4 typography = [
  textSize FontSize.a_14
, lineHeight "18"
]  <> if (getFontType "") == Assets then [fontStyle $ bold LanguageStyle] else [fontWeight $ FontWeight 700] 

body5 ::  LazyCheck -> forall properties. (Array (Prop properties))
body5 typography = [
  textSize FontSize.a_16
]  <> if (getFontType "") == Assets then [fontStyle $ regular LanguageStyle] else [fontWeight $ FontWeight 400]

body6 ::  LazyCheck -> forall properties. (Array (Prop properties))
body6 typography = [
  textSize FontSize.a_14
]  <> if (getFontType "") == Assets then [fontStyle $ semiBold LanguageStyle] else [fontWeight $ FontWeight 600]

body7 ::  LazyCheck -> forall properties. (Array (Prop properties))
body7 typography = [
  textSize FontSize.a_16
]  <> if (getFontType "") == Assets then [fontStyle $ bold LanguageStyle] else [fontWeight $ FontWeight 700]

body8 ::  LazyCheck -> forall properties. (Array (Prop properties))
body8 typography = [
  textSize FontSize.a_20
]  <> if (getFontType "") == Assets then [fontStyle $ bold LanguageStyle] else [fontWeight $ FontWeight 700]

body9 ::  LazyCheck -> forall properties. (Array (Prop properties))
body9 typography = [
  lineHeight "22"
, textSize FontSize.a_12
]  <> if (getFontType "") == Assets then [fontStyle $ semiBold LanguageStyle] else [fontWeight $ FontWeight 600]

body10 ::  LazyCheck -> forall properties. (Array (Prop properties))
body10 typography = [
  textSize FontSize.a_20
]  <> if (getFontType "") == Assets then [fontStyle $ semiBold LanguageStyle] else [fontWeight $ FontWeight 600]

body11 ::  LazyCheck -> forall properties. (Array (Prop properties))
body11 typography = [
  textSize FontSize.a_20
]  <> if (getFontType "") == Assets then [fontStyle $ medium LanguageStyle] else [fontWeight $ FontWeight 500]

body12 ::  LazyCheck -> forall properties. (Array (Prop properties))
body12 typography = [
  textSize FontSize.a_20
]  <> if (getFontType "") == Assets then [fontStyle $ regular LanguageStyle] else [fontWeight $ FontWeight 400]

body13 ::  LazyCheck -> forall properties. (Array (Prop properties))
body13 typography = [
  textSize FontSize.a_18
]  <> if (getFontType "") == Assets then [fontStyle $ medium LanguageStyle] else [fontWeight $ FontWeight 500]

body14 ::  LazyCheck -> forall properties. (Array (Prop properties))
body14 typography = [
  textSize FontSize.a_18
]  <> if (getFontType "") == Assets then [fontStyle $ regular LanguageStyle] else [fontWeight $ FontWeight 400]

body15 ::  LazyCheck -> forall properties. (Array (Prop properties))
body15 typography = [
  textSize FontSize.a_12
]  <> if (getFontType "") == Assets then [fontStyle $ bold LanguageStyle] else [fontWeight $ FontWeight 700]

body16 ::  LazyCheck ->  forall properties. (Array (Prop properties))
body16 typography = [
  textSize FontSize.a_10
]  <> if (getFontType "") == Assets then [fontStyle $ medium LanguageStyle] else [fontWeight $ FontWeight 500]
h0 :: LazyCheck -> forall properties. (Array (Prop properties))
h0 typography = [
  textSize FontSize.a_24
, lineHeight "28"
]  <> if (getFontType "") == Assets then [fontStyle $ bold LanguageStyle] else [fontWeight $ FontWeight 700]

data Style = Body1
  | Body2
  | Body3
  | Body4
  | Body5
  | Body6
  | Body7
  | Body8
  | Body9
  | Body10
  | Body11
  | Body12
  | Body13
  | Body14
  | Body15
  | Body16
  | Heading0
  | Heading1
  | Heading2
  | Heading3
  | SubHeading1
  | SubHeading2
  | Tags
  | ParagraphText
  | Captions
  | PriceFont
  | PriceFontBig


getFontStyle :: Style -> LazyCheck -> forall properties. (Array (Prop properties))
getFontStyle style styleType = case style of
  Body1 -> body1 styleType
  Body2 -> body2 styleType
  Body3 -> body3 styleType
  Body4 -> body4 styleType
  Body5 -> body5 styleType
  Body6 -> body6 styleType
  Body7 -> body7 styleType
  Body8 -> body8 styleType
  Body9 -> body9 styleType
  Body10 -> body10 styleType
  Body11 -> body11 styleType
  Body12 -> body12 styleType
  Body13 -> body13 styleType
  Body14 -> body14 styleType
  Body15 -> body15 styleType
  Body16 -> body16 styleType
  Heading0 -> h0 styleType
  Heading1 -> h1 styleType
  Heading2 -> h2 styleType
  Heading3 -> h3 styleType
  SubHeading1 -> subHeading1 styleType
  SubHeading2 -> subHeading2 styleType
  Tags -> tags styleType
  ParagraphText -> paragraphText styleType
  Captions -> captions styleType
  PriceFont -> priceFont styleType
  PriceFontBig -> priceFont_big styleType