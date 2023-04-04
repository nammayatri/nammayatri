{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SMS.Config where

import EulerHS.Prelude
import Kernel.Utils.Dhall (FromDhall)
import Servant.Client (BaseUrl)

data SmsSessionConfig = SmsSessionConfig
  { attempts :: Int,
    authExpiry :: Int,
    tokenExpiry :: Int
  }
  deriving (Generic, FromDhall)

data SmsCredConfig = SmsCredConfig
  { username :: Text, -- FIXME? Do we need to reuse Servant's one?
    password :: Text, -- idem
    otpHash :: Text
  }
  deriving (Generic, FromDhall)

data SmsConfig = SmsConfig
  { sessionConfig :: SmsSessionConfig,
    credConfig :: SmsCredConfig,
    useFakeSms :: Maybe Word16, -- 4 digit
    url :: BaseUrl,
    sender :: Text
  }
  deriving (Generic, FromDhall)
