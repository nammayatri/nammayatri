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

module Storage.Tabular.Location.TagCategoryMapping where

import qualified Domain.Types.Location.SpecialLocation as Domain
import qualified Domain.Types.Location.TagCategoryMapping as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

derivePersistField "Domain.Category"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    TagCategoryMappingT sql=tag_category_mapping
      id Text
      category Domain.Category
      tag Text
      createdAt UTCTime
      Primary tag
      deriving Generic
    |]

instance TEntityKey TagCategoryMappingT where
  type DomainKey TagCategoryMappingT = Id Domain.TagCategoryMapping
  fromKey (TagCategoryMappingTKey _id) = Id _id
  toKey (Id id) = TagCategoryMappingTKey id

instance FromTType TagCategoryMappingT Domain.TagCategoryMapping where
  fromTType TagCategoryMappingT {..} =
    return
      Domain.TagCategoryMapping
        { id = Id id,
          ..
        }

instance ToTType TagCategoryMappingT Domain.TagCategoryMapping where
  toTType Domain.TagCategoryMapping {..} =
    TagCategoryMappingT
      { id = getId id,
        ..
      }
