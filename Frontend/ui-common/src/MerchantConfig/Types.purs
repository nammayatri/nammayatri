{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Common.Types.Config where

import Common.Types.App
type CommonAppConfig = (
    colors :: Colors
  , primaryButtonConfig :: PrimaryButtonConfig
  , fontConfig :: FontConfig
  , loaderConfig :: LoaderConfig
  , currency :: String
  , internationalNumberEnabled :: Boolean
  , navigationAppConfig :: NavigationConfig
  , genericHeaderConfig :: GenericHeaderConfig
  , showCorporateAddress :: Boolean
  , appData :: AppDatas
  , otpRegex :: String
  , termsLink :: String
  , termsVersion :: Number
  , privacyLink :: String
  , dashboard :: DashboardConfig
  , logFunctionCalls :: Boolean
  , bannerCarousel :: BannerCarousalConfig 
  , countryCodeConfig ::  Array CountryCodeObj
  , defaultLanguage :: String)
  
type Colors = {
  black800 :: String
, black900 :: String
, red :: String
, black700 :: String
, black650 :: String
, black600 :: String
, black500 :: String
, grey900 :: String
, grey800 :: String
, grey700 :: String 
, blue900 :: String 
, blue800 :: String
, blue700 :: String
, blue600 :: String
, green900 :: String
, yellow900 :: String 
, red900 :: String
}

type PrimaryButtonConfig = {
  isGradient :: Boolean
, gradient :: Array String
, loaderUrl :: String
}

type FontConfig = {
    default :: String
  , kannada :: String
  , telugu :: String
  , type :: String
}

type LoaderConfig = {
  color :: String
}

type Miscellaneous = {
  otpRegex :: String
, termsLink :: String
, privacyLink :: String
}

type NavigationConfig = {
  android :: NavigationAppConfig
, ios :: NavigationAppConfig
}

type NavigationAppConfig = {
  query :: String
, packageName :: String
, walkQuery :: String
, directionQuery :: String
, twoWheelerQuery :: String
}

type GenericHeaderConfig = {
  backArrow :: String
}

type AppDatas = {
    link :: String
  , supportMail :: String
  , name :: String
  , website :: String
  , logoLight :: String
}

type Referral = {
    domain :: String
  , customerAppId :: String
  , driverAppId :: String
}

type WaitingChargesConfig = {
  cab :: ChargesEntity,
  auto :: ChargesEntity,
  bike :: ChargesEntity,
  ambulance :: ChargesEntity
}

type ChargesEntity = {
  freeSeconds :: Int,
  perMinCharges :: Number
}

type DashboardConfig = {
    url :: String
  , enable :: Boolean
}

type BannerCarousalConfig = {
  autoScrollDelay :: Number
, enableAutoScroll :: Boolean
}

type GeoJson = {
    type :: String
  , features :: Array GeoJsonFeature
}

type GeoJsonFeature = {
    type :: String
  , properties :: GeoJsonProperties
  , geometry :: GeoJsonGeometry
}

type GeoJsonProperties = {
    name :: String
  , id :: String
  , defaultDriverExtra :: Int
  , canQueueUpOnGate :: Boolean
}

type GeoJsonGeometry = {
    type :: String
  , coordinates :: Array (Array (Array (Array Number)))
}