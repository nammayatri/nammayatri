{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.Person.PersonDisability
  ( PersonDisability (..),
    DisabilityItem (..),
  )
where

import Data.Aeson
import Data.OpenApi hiding (description)
import Domain.Types.Person
import Kernel.Prelude
import Kernel.Types.Id

data PersonDisability = PersonDisability
  { personId :: Id Person,
    disabilityId :: Text,
    tag :: Text,
    description :: Maybe Text,
    updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToSchema, ToJSON, FromJSON)

data DisabilityItem = DisabilityItem
  { id :: Id DisabilityItem,
    tag :: Text,
    description :: Text
  }
  deriving (Show, Eq, Generic, ToSchema, ToJSON, FromJSON)
