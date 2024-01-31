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

module IssueManagement.Storage.Beam.MediaFile where

import AWS.S3 (FileType (..))
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import IssueManagement.Tools.UtilsTH hiding (Generic)
import Kernel.Prelude

data MediaFileT f = MediaFileT
  { id :: B.C f Text,
    fileType :: B.C f FileType,
    url :: B.C f Text,
    createdAt :: B.C f LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MediaFileT where
  data PrimaryKey MediaFileT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type MediaFile = MediaFileT Identity

$(enableKVPG ''MediaFileT ['id] [])

$(mkTableInstancesGenericSchemaWithTModifier ''MediaFileT "media_file" [("fileType", "type")])
