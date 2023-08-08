{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "[]" #-}

module Domain.Types.Person.DisabilityType
  ( DisabilityType (..),
    DisabilityAPIEntity (..),
    makeDisabilityAPIEntity,
  )
where

import Data.Aeson
import Data.OpenApi hiding (description)
import Kernel.Prelude
import Kernel.Types.Id

-- data Disability

data DisabilityType = DisabilityType
  { id :: Id DisabilityType,
    tag :: Text,
    subtag :: Text,
    description :: Maybe Text,
    onBookingMessage :: Maybe Text,
    onArrivalMessage :: Maybe Text,
    onRideStartMessage :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToSchema, ToJSON, FromJSON)

data DisabilityAPIEntity = DisabilityAPIEntity
  { id :: Id DisabilityType,
    tag :: Text,
    subtag :: Text,
    description :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeDisabilityAPIEntity :: DisabilityType -> DisabilityAPIEntity
makeDisabilityAPIEntity DisabilityType {..} =
  DisabilityAPIEntity {..}
