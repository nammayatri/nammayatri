{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Idfy.Types.IdfyConfig where

import Data.Aeson hiding (Error)
import Data.OpenApi hiding (url)
import EulerHS.Prelude hiding (state)
import Kernel.Types.App
import Kernel.Utils.Dhall (FromDhall)

data IdfyConfig = IdfyConfig
  { account_id :: AccountId,
    api_key :: ApiKey,
    secret :: Text,
    url :: BaseUrl
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema, FromDhall)

type AccountId = Text

type ApiKey = Text

data StatusCheck = VALID | INVALID deriving (Generic, ToJSON, Show, FromJSON, ToSchema)
