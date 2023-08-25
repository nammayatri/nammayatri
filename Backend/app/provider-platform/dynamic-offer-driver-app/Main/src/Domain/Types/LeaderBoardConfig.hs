{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.LeaderBoardConfig where

import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import qualified Domain.Types.Merchant as DMerchant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data LeaderBoardType
  = WEEKLY
  | DAILY
  deriving (Generic, ToJSON, FromJSON, ToSchema, Read, Show, Ord, Eq)

instance FromField LeaderBoardType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be LeaderBoardType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be LeaderBoardType

instance FromBackendRow Postgres LeaderBoardType

instance IsString LeaderBoardType where
  fromString = show

data LeaderBoardConfigs = LeaderBoardConfigs
  { id :: Id LeaderBoardConfigs,
    leaderBoardType :: LeaderBoardType,
    numberOfSets :: Int,
    leaderBoardExpiry :: Seconds,
    zScoreBase :: Int,
    leaderBoardLengthLimit :: Integer,
    isEnabled :: Bool,
    merchantId :: Id DMerchant.Merchant
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
