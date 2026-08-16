{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module TempAppCode.Types
  ( CodeStrategy (..),
    TempAppCodeCfg (..),
    TempAppCodeRes (..),
    TempAppCodeValue (..),
  )
where

import Kernel.Prelude

data CodeStrategy
  = -- | A rolling counter reduced modulo N — short and human-readable, and only
    NumericCounter Integer
  | -- | An unguessable id. Required whenever the code alone can redeem.
    Guid
  deriving (Show, Eq)

data TempAppCodeCfg = TempAppCodeCfg
  { -- | Prefix of the code → personId key. Namespaced per platform so the two
    keyPrefix :: Text,
    limitKeyPrefix :: Text,
    counterKey :: Text,
    codeStrategy :: CodeStrategy,
    ttlSeconds :: Int,
    consumeOnRead :: Bool,
    maxAttempts :: Int,
    attemptWindowSeconds :: Int
  }

data TempAppCodeRes = TempAppCodeRes
  { code :: Text,
    expiresAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data TempAppCodeValue = TempAppCodeValue
  { personId :: Text,
    consumeOnRead :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON)
