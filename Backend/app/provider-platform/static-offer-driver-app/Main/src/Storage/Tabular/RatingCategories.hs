{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RatingCategories where

import Beckn.Types.Core.Taxi.Rating.Category
import qualified Domain.Types.RatingCategories as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

derivePersistField "CategoryName"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RatingCategoryT sql=rating_category
      id Text
      category CategoryName
      Primary id
      deriving Generic
    |]

instance TEntityKey RatingCategoryT where
  type DomainKey RatingCategoryT = Id Domain.RatingCategory
  fromKey (RatingCategoryTKey _id) = Id _id
  toKey (Id id) = RatingCategoryTKey id

instance FromTType RatingCategoryT Domain.RatingCategory where
  fromTType RatingCategoryT {..} = do
    return $
      Domain.RatingCategory
        { id = Id id,
          ..
        }

instance ToTType RatingCategoryT Domain.RatingCategory where
  toTType Domain.RatingCategory {..} =
    RatingCategoryT
      { id = getId id,
        ..
      }
