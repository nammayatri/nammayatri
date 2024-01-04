{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.SearchRequestMapping where

import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.SearchRequestMapping as DLM
import Kernel.Prelude
import Tools.Beam.UtilsTH

data SearchRequestMappingT f = SearchRequestMappingT
  { id :: B.C f Text,
    tag :: B.C f DLM.SearchRequestMappingTags,
    locationId :: B.C f Text,
    entityId :: B.C f Text,
    order :: B.C f Int,
    version :: B.C f Text,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime,
    merchantId :: B.C f (Maybe Text),
    merchantOperatingCityId :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchRequestMappingT where
  data PrimaryKey SearchRequestMappingT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type SearchRequestMapping = SearchRequestMappingT Identity

$(enableKVPG ''SearchRequestMappingT ['id] [['entityId]])

$(mkTableInstances ''SearchRequestMappingT "search_request_mapping")
