{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Font.Style where

import Constants
import Data.Generic.Rep (class Generic)
import Font.Size as FontSize
import Foreign.Generic (class Decode)
import Halogen.VDom.DOM.Prop (Prop)
import Helpers.Commons (os)
import Locale.Utils (getLanguageLocale)
import Prelude (Unit, unit, ($), (<>), (==))
import Presto.Core.Utils.Encoding (defaultDecode)
import PrestoDOM (fontStyle, lineHeight, textSize)
import Types (LazyCheck(..))

type FontStyle
  = String

getLanguageFromLocalStore :: Unit -> String
getLanguageFromLocalStore _ = getLanguageLocale languageKey

italic :: FontStyle
italic = do
  let
    font = getDefaultFont TypoGraphy
  fontByOS (font <> "-Italic") (font <> "-Italic") "Arial"

light :: LazyCheck -> FontStyle
light style = do
  let
    font = getDefaultFont style
  let
    fontKn = getKannadaFont style

    fontTe = getTeluguFont style
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS (font <> "-Light") (font <> "-Light") "Arial"
    "KN_IN" -> fontByOS (fontKn <> "-Light") (fontKn <> "-Light") "Arial"
    "TE_IN" -> fontByOS (fontTe <> "-Light") (fontTe <> "-Light") "Arial"
    "HI_IN" -> fontByOS (font <> "-Light") (font <> "-Light") "Arial"
    _ -> fontByOS (font <> "-Light") (font <> "-Light") "Arial"

lightItalic :: FontStyle
lightItalic = do
  let
    font = getDefaultFont TypoGraphy
  fontByOS (font <> "-LightItalic") (font <> "-LightItalic") "Arial"

extraLight :: LazyCheck -> FontStyle
extraLight style = do
  let
    font = getDefaultFont style
  let
    fontKn = getKannadaFont style

    fontTe = getTeluguFont style
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS (font <> "-ExtraLight") (font <> "-ExtraLight") "Arial"
    "KN_IN" -> fontByOS (fontKn <> "-ExtraLight") (fontKn <> "-ExtraLight") "Arial"
    "TE_IN" -> fontByOS (fontTe <> "-ExtraLight") (fontTe <> "-ExtraLight") "Arial"
    "HI_IN" -> fontByOS (font <> "-ExtraLight") (font <> "-ExtraLight") "Arial"
    _ -> fontByOS (font <> "-ExtraLight") (font <> "-ExtraLight") "Arial"

extraLightItalic :: FontStyle
extraLightItalic = do
  let
    font = getDefaultFont TypoGraphy
  fontByOS (font <> "-ExtraLightItalic") (font <> "-ExtraLightItalic") "Arial"

regular :: LazyCheck -> FontStyle
regular style = do
  let
    font = getDefaultFont style
  let
    fontKn = getKannadaFont style

    fontTe = getTeluguFont style
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS (font <> "-Regular") (font <> "-Regular") "Arial"
    "KN_IN" -> fontByOS (fontKn <> "-Regular") (fontKn <> "-Regular") "Arial"
    "TE_IN" -> fontByOS (fontTe <> "-Regular") (fontTe <> "-Regular") "Arial"
    "HI_IN" -> fontByOS (font <> "-Regular") (font <> "-Regular") "Arial"
    _ -> fontByOS (font <> "-Regular") (font <> "-Regular") "Arial"

medium :: LazyCheck -> FontStyle
medium style = do
  let
    font = getDefaultFont style
  let
    fontKn = getKannadaFont style

    fontTe = getTeluguFont style
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS (font <> "-Medium") (font <> "-Medium") "Arial"
    "KN_IN" -> fontByOS (fontKn <> "-Medium") (fontKn <> "-Medium") "Arial"
    "TE_IN" -> fontByOS (fontTe <> "-Medium") (fontTe <> "-Medium") "Arial"
    "HI_IN" -> fontByOS (font <> "-Medium") (font <> "-Medium") "Arial"
    _ -> fontByOS (font <> "-Medium") (font <> "-Medium") "Arial"

mediumItalic :: FontStyle
mediumItalic = do
  let
    font = getDefaultFont TypoGraphy
  fontByOS (font <> "-MediumItalic") (font <> "-MediumItalic") "Arial"

semiBold :: LazyCheck -> FontStyle
semiBold style = do
  let
    font = getDefaultFont style
  let
    fontKn = getKannadaFont style

    fontTe = getTeluguFont style
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS (font <> "-SemiBold") (font <> "-SemiBold") "Arial"
    "KN_IN" -> fontByOS (fontKn <> "-SemiBold") (fontKn <> "-SemiBold") "Arial"
    "TE_IN" -> fontByOS (fontTe <> "-SemiBold") (fontTe <> "-SemiBold") "Arial"
    "HI_IN" -> fontByOS (font <> "-SemiBold") (font <> "-SemiBold") "Arial"
    _ -> fontByOS (font <> "-SemiBold") (font <> "-SemiBold") "Arial"

semiBoldItalic :: FontStyle
semiBoldItalic = do
  let
    font = getDefaultFont TypoGraphy
  fontByOS (font <> "-SemiBoldItalic") (font <> "-SemiBoldItalic") "Arial"

bold :: LazyCheck -> FontStyle
bold style = do
  let
    font = getDefaultFont style
  let
    fontKn = getKannadaFont style

    fontTe = getTeluguFont style
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS (font <> "-Bold") (font <> "-Bold") "Arial"
    "KN_IN" -> fontByOS (fontKn <> "-Bold") (fontKn <> "-Bold") "Arial"
    "TE_IN" -> fontByOS (fontTe <> "-Bold") (fontTe <> "-Bold") "Arial"
    "HI_IN" -> fontByOS (font <> "-Bold") (font <> "-Bold") "Arial"
    _ -> fontByOS (font <> "-Bold") (font <> "-Bold") "Arial"

boldItalic :: FontStyle
boldItalic = do
  let
    font = getDefaultFont TypoGraphy
  fontByOS (font <> "-BoldItalic") (font <> "-BoldItalic") "Arial"

extraBold :: LazyCheck -> FontStyle
extraBold style = do
  let
    font = getDefaultFont style
  let
    fontKn = getKannadaFont style

    fontTe = getTeluguFont style
  case (getLanguageFromLocalStore unit) of
    "EN_US" -> fontByOS (font <> "-ExtraBold") (font <> "-ExtraBold") "Arial"
    "KN_IN" -> fontByOS (fontKn <> "-ExtraBold") (fontKn <> "-ExtraBold") "Arial"
    "TE_IN" -> fontByOS (fontTe <> "-ExtraBold") (fontTe <> "-ExtraBold") "Arial"
    "HI_IN" -> fontByOS (font <> "-ExtraBold") (font <> "-ExtraBold") "Arial"
    _ -> fontByOS (font <> "-ExtraBold") (font <> "-ExtraBold") "Arial"

extraBoldItalic :: FontStyle
extraBoldItalic = do
  let
    font = getDefaultFont TypoGraphy
  fontByOS (font <> "-ExtraBoldItalic") (font <> "-ExtraBoldItalic") "Arial"

feFont :: LazyCheck -> FontStyle
feFont _ = fontByOS "FEFont" "FE-Font" "Arial"

h1 :: LazyCheck -> forall properties. (Array (Prop properties))
h1 typography =
  [ textSize FontSize.a_22
  , lineHeight "28"
  , fontStyle $ bold typography
  ]

h2 :: LazyCheck -> forall properties. (Array (Prop properties))
h2 typography =
  [ textSize FontSize.a_18
  , lineHeight "23"
  , fontStyle $ bold typography
  ]

h3 :: LazyCheck -> forall properties. (Array (Prop properties))
h3 typography =
  [ textSize FontSize.a_18
  , lineHeight "23"
  , fontStyle $ semiBold typography
  ]

subHeading1 :: LazyCheck -> forall properties. (Array (Prop properties))
subHeading1 typography =
  [ textSize if (getLanguageFromLocalStore unit) == "TA_IN" then FontSize.a_13 else FontSize.a_16
  , lineHeight "24"
  , fontStyle $ semiBold typography
  ]

subHeading3 :: LazyCheck -> forall properties. (Array (Prop properties))
subHeading3 typography =
  [ textSize FontSize.a_16
  , lineHeight "20"
  , fontStyle $ semiBold typography
  ]

subHeading2 :: LazyCheck -> forall properties. (Array (Prop properties))
subHeading2 typography =
  [ textSize FontSize.a_16
  , lineHeight "24"
  , fontStyle $ medium typography
  ]

body1 :: LazyCheck -> forall properties. (Array (Prop properties))
body1 typography =
  [ textSize $ if (getLanguageFromLocalStore unit) == "TA_IN" then FontSize.a_12 else FontSize.a_14
  , lineHeight "18"
  , fontStyle $ medium typography
  ]

body2 :: LazyCheck -> forall properties. (Array (Prop properties))
body2 typography =
  [ textSize FontSize.a_14
  , lineHeight "20"
  , fontStyle $ medium typography
  ]

body20 :: LazyCheck -> forall properties. (Array (Prop properties))
body20 typography =
  [ textSize FontSize.a_14
  , lineHeight "18"
  , fontStyle $ medium typography
  ]

body3 :: LazyCheck -> forall properties. (Array (Prop properties))
body3 typography =
  [ textSize FontSize.a_12
  , lineHeight "16"
  , fontStyle $ regular typography
  ]

paragraphText :: LazyCheck -> forall properties. (Array (Prop properties))
paragraphText typography =
  [ textSize FontSize.a_14
  , lineHeight "18"
  , fontStyle $ regular typography
  ]

tags :: LazyCheck -> forall properties. (Array (Prop properties))
tags typography =
  [ textSize FontSize.a_12
  , lineHeight "15"
  , fontStyle $ medium typography
  ]

captions :: LazyCheck -> forall properties. (Array (Prop properties))
captions typography =
  [ textSize FontSize.a_10
  , lineHeight "13"
  , fontStyle $ regular typography
  ]

body21 :: LazyCheck -> forall properties. (Array (Prop properties))
body21 typography =
  [ textSize FontSize.a_10
  , fontStyle $ regular typography
  ]

priceFont :: LazyCheck -> forall properties. (Array (Prop properties))
priceFont typography =
  [ textSize FontSize.a_40
  , lineHeight "40"
  , fontStyle $ bold typography
  ]

priceFont_big :: LazyCheck -> forall properties. (Array (Prop properties))
priceFont_big typography =
  [ textSize FontSize.a_44
  , lineHeight "40"
  , fontStyle $ bold typography
  ]

fontByOS :: forall a. a -> a -> a -> a
fontByOS android ios web = case os of
  "IOS" -> ios
  "WEB" -> web
  _ -> android

body4 :: LazyCheck -> forall properties. (Array (Prop properties))
body4 typography =
  [ textSize FontSize.a_14
  , lineHeight "18"
  , fontStyle $ bold typography
  ]

body5 :: LazyCheck -> forall properties. (Array (Prop properties))
body5 typography =
  [ textSize FontSize.a_16
  , fontStyle $ regular typography
  ]

body6 :: LazyCheck -> forall properties. (Array (Prop properties))
body6 typography =
  [ textSize FontSize.a_14
  , fontStyle $ semiBold typography
  ]

body7 :: LazyCheck -> forall properties. (Array (Prop properties))
body7 typography =
  [ textSize if (getLanguageFromLocalStore unit) == "TA_IN" then FontSize.a_14 else FontSize.a_16
  , lineHeight "20"
  , fontStyle $ bold typography
  ]

body8 :: LazyCheck -> forall properties. (Array (Prop properties))
body8 typography =
  [ textSize FontSize.a_20
  , fontStyle $ bold typography
  ]

body9 :: LazyCheck -> forall properties. (Array (Prop properties))
body9 typography =
  [ lineHeight "22"
  , textSize FontSize.a_12
  , fontStyle $ semiBold typography
  ]

body10 :: LazyCheck -> forall properties. (Array (Prop properties))
body10 typography =
  [ textSize FontSize.a_20
  , fontStyle $ semiBold typography
  ]

body11 :: LazyCheck -> forall properties. (Array (Prop properties))
body11 typography =
  [ textSize FontSize.a_20
  , fontStyle $ medium typography
  ]

body12 :: LazyCheck -> forall properties. (Array (Prop properties))
body12 typography =
  [ textSize FontSize.a_20
  , fontStyle $ regular typography
  ]

body13 :: LazyCheck -> forall properties. (Array (Prop properties))
body13 typography =
  [ textSize FontSize.a_18
  , fontStyle $ medium typography
  ]

body14 :: LazyCheck -> forall properties. (Array (Prop properties))
body14 typography =
  [ textSize FontSize.a_18
  , fontStyle $ regular typography
  ]

body15 :: LazyCheck -> forall properties. (Array (Prop properties))
body15 typography =
  [ textSize FontSize.a_12
  , fontStyle $ bold typography
  ]

body16 :: LazyCheck -> forall properties. (Array (Prop properties))
body16 typography =
  [ textSize FontSize.a_10
  , fontStyle $ medium typography
  ]

body17 :: LazyCheck -> forall properties. (Array (Prop properties))
body17 typography =
  [ textSize FontSize.a_10
  , fontStyle $ semiBold typography
  ]

body19 :: LazyCheck -> forall properties. (Array (Prop properties))
body19 typography =
  [ textSize FontSize.a_10
  , fontStyle $ bold typography
  ]

body18 :: LazyCheck -> forall properties. (Array (Prop properties))
body18 typography =
  [ textSize FontSize.a_8
  , fontStyle $ semiBold typography
  ]

body22 :: LazyCheck -> forall properties. (Array (Prop properties))
body22 typography =
  [ textSize FontSize.a_14
  , fontStyle $ bold typography
  ]

body23 :: LazyCheck -> forall properties. (Array (Prop properties))
body23 typography =
  [ textSize FontSize.a_16
  , lineHeight "19"
  , fontStyle $ semiBold typography
  ]

body24 :: LazyCheck -> forall properties. (Array (Prop properties))
body24 typography =
  [ lineHeight "12"
  , textSize FontSize.a_12
  , fontStyle $ semiBold typography
  ]

body25 :: LazyCheck -> forall properties. (Array (Prop properties))
body25 typography =
  [ lineHeight "20"
  , textSize FontSize.a_16
  , fontStyle $ semiBold typography
  ]

body26 :: LazyCheck -> forall properties. (Array (Prop properties))
body26 typography =
  [ textSize FontSize.a_14
  , lineHeight "15"
  , fontStyle $ bold typography
  ]

body27 :: LazyCheck -> forall properties. (Array (Prop properties))
body27 typography =
  [ textSize FontSize.a_12
  , fontStyle $ semiBold typography
  ]

h0 :: LazyCheck -> forall properties. (Array (Prop properties))
h0 typography =
  [ textSize FontSize.a_24
  , lineHeight "28"
  , fontStyle $ bold typography
  ]

h4 :: LazyCheck -> forall properties. (Array (Prop properties))
h4 typography = [ fontStyle $ feFont typography, textSize $ FontSize.a_44 ]

title0 :: LazyCheck -> forall properties. (Array (Prop properties))
title0 typography =
  [ textSize FontSize.a_72
  , fontStyle $ bold typography
  ]

title1 :: LazyCheck -> forall properties. (Array (Prop properties))
title1 typography =
  [ textSize FontSize.a_28
  , lineHeight "40"
  , fontStyle $ medium typography
  ]

title2 :: LazyCheck -> forall properties. (Array (Prop properties))
title2 typography =
  [ textSize FontSize.a_32
  , lineHeight "40"
  , fontStyle $ bold typography
  ]

data Style
  = Body1
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
  | Body17
  | Body18
  | Heading0
  | Heading1
  | Heading2
  | Heading3
  | Heading4
  | SubHeading1
  | SubHeading2
  | Tags
  | ParagraphText
  | Captions
  | PriceFont
  | PriceFontBig
  | Title0
  | Title1

derive instance genericStyle :: Generic Style _

instance decodeStyle :: Decode Style where
  decode = defaultDecode

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
  Body17 -> body17 styleType
  Body18 -> body18 styleType
  Heading0 -> h0 styleType
  Heading1 -> h1 styleType
  Heading2 -> h2 styleType
  Heading3 -> h3 styleType
  Heading4 -> h4 styleType
  SubHeading1 -> subHeading1 styleType
  SubHeading2 -> subHeading2 styleType
  Tags -> tags styleType
  ParagraphText -> paragraphText styleType
  Captions -> captions styleType
  PriceFont -> priceFont styleType
  PriceFontBig -> priceFont_big styleType
  Title0 -> title0 styleType
  Title1 -> title1 styleType

getDefaultFont :: LazyCheck -> String
getDefaultFont _ = "PlusJakartaSans"

getKannadaFont :: LazyCheck -> String
getKannadaFont _ = "NotoSansKannada"

getTeluguFont :: LazyCheck -> String
getTeluguFont _ = "NotoSansTelugu"
