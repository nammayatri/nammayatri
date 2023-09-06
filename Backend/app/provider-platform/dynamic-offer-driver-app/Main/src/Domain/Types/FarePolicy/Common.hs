{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.FarePolicy.Common where

import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForJSON)
import Kernel.Prelude
import Kernel.Types.Common

data WaitingChargeInfo = WaitingChargeInfo
  { freeWaitingTime :: Minutes,
    waitingCharge :: WaitingCharge
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema, Read)

instance FromField WaitingChargeInfo where
  fromField = fromFieldJSON

instance HasSqlValueSyntax be String => HasSqlValueSyntax be WaitingChargeInfo where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be WaitingChargeInfo

instance FromBackendRow Postgres WaitingChargeInfo

data WaitingCharge = PerMinuteWaitingCharge HighPrecMoney | ConstantWaitingCharge Money
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForJSON ''WaitingCharge)

data NightShiftCharge = ProgressiveNightShiftCharge Float | ConstantNightShiftCharge Money
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForJSON ''NightShiftCharge)
