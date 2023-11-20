{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.LeaderBoardConfig where

import qualified Domain.Types.Merchant as DMerchant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data LeaderBoardType
  = WEEKLY
  | DAILY
  deriving (Generic, ToJSON, FromJSON, ToSchema, Read, Show, Ord, Eq)

$(mkBeamInstancesForEnum ''LeaderBoardType)

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
