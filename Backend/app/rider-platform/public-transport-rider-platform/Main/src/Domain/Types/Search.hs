{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Search where

import Kernel.Prelude
import Kernel.Types.Id

data Search = Search
  { id :: Id Search,
    lat :: Double,
    lon :: Double,
    requestorId :: Id Text,
    createdAt :: UTCTime
  }
  deriving (Generic)

data SearchAPIEntity = SearchAPIEntity
  { id :: Id Search,
    lat :: Double,
    lon :: Double,
    requestorId :: Id Text
  }
  deriving (Generic, ToJSON)

makeSearchAPIEntity :: Search -> SearchAPIEntity
makeSearchAPIEntity Search {..} = do
  SearchAPIEntity
    { ..
    }
