module RemoteConfigs where

import Prelude
import DecodeUtil (decodeForeignObject, parseJSON)
import Foreign (Foreign)
import Foreign.Index (readProp)
import RemoteConfig.Utils (fetchRemoteConfigString)
import Data.Maybe (Maybe(..))
import Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Presto.Core.Utils.Encoding (defaultDecode)
import Control.Monad.Except (runExcept)

foreign import parseJsonConfig :: String -> Foreign

type SafetyVideoConfig
  = { videoId :: String
    , title :: String
    , coverImageUrl :: String
    }

safetyVideoConfig :: Array SafetyVideoConfig
safetyVideoConfig = do
  let
    conf = parseJsonConfig $ fetchRemoteConfigString "safety_videos"
  decodeForeignObject conf defaultVideoConfig

defaultVideoConfig :: Array SafetyVideoConfig
defaultVideoConfig = []
