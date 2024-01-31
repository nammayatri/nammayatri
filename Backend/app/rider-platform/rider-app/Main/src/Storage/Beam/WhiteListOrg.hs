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

module Storage.Beam.WhiteListOrg where

import qualified Database.Beam as B
import qualified Domain.Types.WhiteListOrg as Domain
import Kernel.Prelude
import Tools.Beam.UtilsTH

data WhiteListOrgT f = WhiteListOrgT
  { id :: B.C f Text,
    subscriberId :: B.C f Text,
    orgType :: B.C f Domain.WhiteListOrgType
  }
  deriving (Generic, B.Beamable)

instance B.Table WhiteListOrgT where
  data PrimaryKey WhiteListOrgT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type WhiteListOrg = WhiteListOrgT Identity

$(enableKVPG ''WhiteListOrgT ['id] [['subscriberId]])

$(mkTableInstancesWithTModifier ''WhiteListOrgT "white_list_org" [("orgType", "type")])
