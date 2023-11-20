{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module AssetsProvider where

import Prelude
import Constants as Constants
import Control.Monad.Except (runExcept)
import Data.Array (catMaybes, cons, fold, foldr)
import Data.Array.NonEmpty (head)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, Fn2, Fn3, mkFn1, mkFn2, mkFn3, runFn1, runFn2, runFn3)
import Data.Generic.Rep (class Generic)
import Data.Maybe as Maybe
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Effect.Exception (throw)
import Effect.Uncurried (EffectFn1, EffectFn3, mkEffectFn1, runEffectFn1, runEffectFn3)
import Foreign (Foreign)
import Foreign.Generic (class Decode, decode)
import Foreign.Object (Object, lookup)
import Presto.Core.Utils.Encoding (defaultDecode)
import ConfigProvider (loadFileInDUI)
import Debug

foreign import getFromTopWindow :: EffectFn1 String Foreign

foreign import getCUGUser :: Fn1 Unit Boolean

foreign import isUseLocalAssets :: Fn1 Unit Boolean

foreign import renewFile :: EffectFn3 String String (Boolean -> Effect Unit) Unit

-- JSON Utils
foreign import parseJSON :: String -> Foreign

fetchAssets :: Effect Unit
fetchAssets = do
  if runFn1 isUseLocalAssets unit then
    pure unit
  else do
    config <- runEffectFn1 getFromTopWindow "configPackage"
    (decodedConfig :: AssetConfig) <- case runExcept (decode config) of
      Right dConfig -> pure dConfig
      Left _ -> do
        configFile <- runEffectFn1 loadFileInDUI $ "config" <> Constants.dotJSON
        case runExcept (decode $ parseJSON $ configFile) of
          Right dConfig -> pure dConfig
          Left _ -> throw "prefetch_failed"
    files <- runEffectFn1 getDownloadList decodedConfig
    fold $ map (\item -> launchAff_ $ void $ download item.path item.location) files

download :: String -> String -> Aff Boolean
download filepath location = makeAff \cb -> runEffectFn3 renewFile filepath location (cb <<< Right) $> nonCanceler

getRoot :: Fn2 (Object (Object Dependency)) String String
getRoot =
  mkFn2 \dependency app -> do
    let
      currentApp = lookup app dependency
    case currentApp of
      Maybe.Nothing -> ""
      Maybe.Just value -> do
        let
          default = lookup "default" value
        case default of
          Maybe.Nothing -> ""
          Maybe.Just (Dependency currDependency) -> currDependency.root

getDownloadList :: EffectFn1 AssetConfig (Array RenewFile)
getDownloadList =
  mkEffectFn1 \(AssetConfig config) -> do
    let
      apps = config.app_list

      isCUGUser = runFn1 getCUGUser unit

      (AssetBlock currentAssetBlock) = if isCUGUser then config.new else config.live
    pure
      $ foldr
          ( \app acc -> do
              let
                _ = app
              let
                _ = acc
              let
                assets = lookup app currentAssetBlock.assets
              let
                bundle = lookup app currentAssetBlock.package
              let
                root = runFn2 getRoot config.dependencies app
              if Maybe.isNothing assets || Maybe.isNothing bundle then
                acc
              else do acc <> runFn3 getAssetFiles assets bundle root
          )
          []
          apps

getAssetFiles ∷ Fn3 (Maybe.Maybe AssetsListBlock) (Maybe.Maybe String) String (Array RenewFile)
getAssetFiles =
  mkFn3 \assets bundle root -> do
    let
      assetsList = catMaybes $ getAssetListBlock (Maybe.fromMaybe emptyAssets assets)
    case bundle of
      Maybe.Nothing -> []
      Maybe.Just currentBundle -> do
        let
          assetsFiles = map (\assetUrl -> runFn2 getRenewFile root assetUrl) assetsList
        cons (runFn2 getRenewFile root currentBundle) assetsFiles

getAssetListBlock :: AssetsListBlock -> Array (Maybe.Maybe String)
getAssetListBlock (AssetsListBlock assets) = [ assets.configuration, assets.config, assets.icons, assets.strings ]

getRenewFile :: Fn2 String String RenewFile
getRenewFile =
  mkFn2 \root url ->
    { location: url
    , path: root <> getFileNameFromUrl url
    }

getFileNameFromUrl :: Fn1 String String
getFileNameFromUrl =
  mkFn1 \url -> case regex "(v1-[^/]*\\.((zip)|(jsa)))|[^/]*\\.(html|js)" noFlags of
    Left _ -> url
    Right r -> case match r url of
      Maybe.Nothing -> url
      Maybe.Just results -> Maybe.fromMaybe "" $ head results

type RenewFile
  = { location :: String
    , path :: String
    }

newtype AssetConfig
  = AssetConfig
  { app_list :: Array String
  , dependencies :: Object (Object Dependency)
  , new :: AssetBlock
  , live :: AssetBlock
  }

newtype Dependency
  = Dependency
  { root :: String
  }

newtype AssetBlock
  = AssetBlock
  { package_version :: String
  , package :: Object String
  , id :: String
  , assets :: Object AssetsListBlock
  }

newtype AssetsListBlock
  = AssetsListBlock
  { configuration :: Maybe.Maybe String
  , config :: Maybe.Maybe String
  , icons :: Maybe.Maybe String
  , strings :: Maybe.Maybe String
  }

derive instance genericAssetConfig :: Generic AssetConfig _

derive instance newtypeAssetConfig :: Newtype AssetConfig _

instance showAssetConfig :: Show AssetConfig where
  show = genericShow

instance decodeAssetConfig :: Decode AssetConfig where
  decode = defaultDecode

derive instance genericDependency :: Generic Dependency _

derive instance newtypeDependency :: Newtype Dependency _

instance showDependency :: Show Dependency where
  show = genericShow

instance decodeDependency :: Decode Dependency where
  decode = defaultDecode

derive instance genericAssetBlock :: Generic AssetBlock _

derive instance newtypeAssetBlock :: Newtype AssetBlock _

instance showAssetBlock :: Show AssetBlock where
  show = genericShow

instance decodeAssetBlock :: Decode AssetBlock where
  decode = defaultDecode

derive instance genericAssetsListBlock :: Generic AssetsListBlock _

derive instance newtypeAssetsListBlock :: Newtype AssetsListBlock _

instance showAssetsListBlock :: Show AssetsListBlock where
  show = genericShow

instance decodeAssetsListBlock :: Decode AssetsListBlock where
  decode = defaultDecode

emptyAssets :: AssetsListBlock
emptyAssets =
  AssetsListBlock
    { configuration: Maybe.Nothing
    , config: Maybe.Nothing
    , icons: Maybe.Nothing
    , strings: Maybe.Nothing
    }
