module RemoteConfigs where

import Prelude
import DecodeUtil (decodeForeignObject)
import Foreign (Foreign)
import RemoteConfig.Utils (fetchRemoteConfigString)

foreign import getSubsRemoteConfig :: String -> Foreign

subscriptionConfig :: String -> SubsRemoteConfig
subscriptionConfig key = do
    let conf = getSubsRemoteConfig $ fetchRemoteConfigString key
    decodeForeignObject conf subscriptionRemoteConfig

type SubsRemoteConfig = {
    max_dues_limit :: Number,
    low_dues_warning_limit :: Number,
    high_due_warning_limit :: Number
}

subscriptionRemoteConfig :: SubsRemoteConfig
subscriptionRemoteConfig = {
    max_dues_limit : 100.0,
    low_dues_warning_limit : 25.0,
    high_due_warning_limit : 75.0
}