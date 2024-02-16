{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module RemoteConfig.Utils where

import Common.RemoteConfig (fetchRemoteConfigString)

import Prelude
import DecodeUtil (decodeForeignObject, parseJSON)
import Foreign (Foreign)
import Foreign.Index (readProp)
import Data.Newtype (class Newtype)
import Presto.Core.Utils.Encoding (defaultDecode)
import RemoteConfig.Types (RCSubscription)

foreign import getSubsRemoteConfig :: String -> Foreign



subscriptionRemoteConfig :: RCSubscription
subscriptionRemoteConfig = {
    max_dues_limit : 100.0,
    low_dues_warning_limit : 25.0,
    high_due_warning_limit : 75.0
}

subscriptionConfig :: String -> RCSubscription
subscriptionConfig key = do
    let conf = getSubsRemoteConfig $ fetchRemoteConfigString key
    decodeForeignObject conf subscriptionRemoteConfig