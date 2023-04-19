{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.CancellationReasons.Types where

import Data.OpenApi (ToSchema)
import Data.OpenApi.Internal.ParamSchema (ToParamSchema)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Beckn.ReqTypes

newtype CancellationReasonCode = CancellationReasonCode Text
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data CancellationReason = CancellationReason
  { reasonCode :: CancellationReasonCode,
    description :: Text,
    enabled :: Bool,
    priority :: Int
  }
  deriving (Generic, ToJSON, ToSchema, Show, FromJSON)

type CancellationReasons = [CancellationReason]

type CancellationReasonsReq = BecknReq Empty

data CancellationReasonAPIEntity = CancellationReasonAPIEntity
  { reasonCode :: CancellationReasonCode,
    description :: Text
  }
  deriving (Generic, ToJSON, ToSchema, Show, FromJSON)

makeCancellationReasonAPIEntity :: CancellationReason -> CancellationReasonAPIEntity
makeCancellationReasonAPIEntity CancellationReason {..} =
  CancellationReasonAPIEntity {..}

newtype CancellationReasonsMessage = CancellationReasonsMessage {cancellation_reasons :: [CancellationReasonInfo]}
  deriving (Generic, ToJSON, ToSchema, Show, FromJSON)

data CancellationReasonInfo = CancellationReasonInfo {id :: Int, descriptor :: Name}
  deriving (Generic, ToJSON, ToSchema, Show, FromJSON)

newtype Name = Name {name :: Text} deriving (Generic, ToJSON, ToSchema, Show, FromJSON)

makeCancellationReasonsMessage :: CancellationReasons -> CancellationReasonsMessage
makeCancellationReasonsMessage listOfReasons = CancellationReasonsMessage results
  where
    toInfoList (x : xs) n = CancellationReasonInfo n (Name x.description) : toInfoList xs (n + 1)
    toInfoList [] _ = []

    results = toInfoList listOfReasons 1

type CancellationReasonsRes = BecknReq CancellationReasonsMessage
