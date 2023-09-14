{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Helpers.FileProvider.Utils where

import Prelude
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn1, runEffectFn2)
import Data.Function.Uncurried (Fn1)
import Foreign (Foreign)
import Foreign.Generic (encode)
import MerchantConfigs.CommonConfig (commonConfig)
import Constants as Constants

foreign import mergeforegin :: EffectFn1 (Array Foreign) Foreign

foreign import appendConfigToDocument :: EffectFn1 String String

foreign import loadInWindow :: forall a. EffectFn2 String a Unit

foreign import loadFileInDUI :: EffectFn1 String String

foreign import stringifyJSON :: forall a. Fn1 a String



loadAppConfig :: EffectFn1 String Unit
loadAppConfig =
  mkEffectFn1 \_ -> do
    commonConfig <- commonConfig
    merchantConfig <- runEffectFn1 getConfigFromFile Constants.configuration_file
    mergedConfig <- runEffectFn1 mergeObjects [ commonConfig, merchantConfig ]
    runEffectFn2 loadInWindow Constants.appConfig mergedConfig

getConfigFromFile :: EffectFn1 String Foreign
getConfigFromFile =
  mkEffectFn1 \fileName -> do
    config <- runEffectFn1 loadFileInDUI $ fileName <> Constants.dotJSA
    if isFilePresent config then do
      merchantConfig <- runEffectFn1 appendConfigToDocument config
      pure $ encode merchantConfig
    else do
      jsConfig <- getConfigFromJS fileName
      if isFilePresent jsConfig then do
        merchantConfigjs <- runEffectFn1 appendConfigToDocument jsConfig
        pure $ encode merchantConfigjs
      else
        throw $ fileName <> " is not present"

-- First element is base object and priority increses with increase in descendants. Keys in high priority elements will be overrided in base object.
mergeObjects :: EffectFn1 (Array Foreign) Foreign
mergeObjects = mkEffectFn1 \arrayObjects -> runEffectFn1 mergeforegin arrayObjects

getConfigFromJS :: String -> Effect String
getConfigFromJS fileName = runEffectFn1 loadFileInDUI $ fileName <> Constants.dotJS

isFilePresent :: String -> Boolean
isFilePresent file = file /= "" && file /= "undefined"
