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

module Storage.Beam.Merchant.LeaderBoardConfig where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Merchant.LeaderBoardConfig as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize
import Tools.Beam.UtilsTH

data LeaderBoardConfigsT f = LeaderBoardConfigsT
  { id :: B.C f Text,
    leaderBoardType :: B.C f Domain.LeaderBoardType,
    numberOfSets :: B.C f Int,
    leaderBoardExpiry :: B.C f Seconds,
    zScoreBase :: B.C f Int,
    leaderBoardLengthLimit :: B.C f Int,
    isEnabled :: B.C f Bool,
    merchantId :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table LeaderBoardConfigsT where
  data PrimaryKey LeaderBoardConfigsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type LeaderBoardConfigs = LeaderBoardConfigsT Identity

$(enableKVPG ''LeaderBoardConfigsT ['id] [['merchantId, 'leaderBoardType]])

$(mkTableInstances ''LeaderBoardConfigsT "leader_board_configs")
