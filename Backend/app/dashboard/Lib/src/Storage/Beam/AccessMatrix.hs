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
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.AccessMatrix where

import qualified Data.Time as Time
import qualified Database.Beam as B
import qualified Domain.Types.AccessMatrix as Domain
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

data AccessMatrixT f = AccessMatrixT
  { id :: B.C f Text,
    roleId :: B.C f Text,
    apiEntity :: B.C f Domain.ApiEntity,
    userActionType :: B.C f Domain.UserActionType,
    userAccessType :: B.C f Domain.UserAccessType,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table AccessMatrixT where
  data PrimaryKey AccessMatrixT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type AccessMatrix = AccessMatrixT Identity

$(enableKVPG ''AccessMatrixT ['id] [])

$(mkTableInstancesGenericSchema ''AccessMatrixT "access_matrix")
