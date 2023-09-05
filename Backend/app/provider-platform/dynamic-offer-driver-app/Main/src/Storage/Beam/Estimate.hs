{-# LANGUAGE DerivingStrategies #-}
{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Storage.Beam.Estimate where

import qualified Data.Aeson as A
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import Domain.Types.Common (UsageSafety (..))
import qualified Domain.Types.Estimate as Domain
import qualified Domain.Types.Vehicle as Variant
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Sequelize
import Tools.Beam.UtilsTH

newtype TimeOfDayText = TimeOfDayText TimeOfDay
  deriving newtype (Eq, Read, Show, Ord, A.FromJSON, A.ToJSON)

$(mkBeamInstancesForEnum ''TimeOfDayText)

data EstimateT f = EstimateT
  { id :: B.C f Text,
    requestId :: B.C f Text,
    vehicleVariant :: B.C f Variant.Variant,
    minFare :: B.C f Money,
    maxFare :: B.C f Money,
    estimateBreakupList :: B.C f [Domain.EstimateBreakupD 'Unsafe],
    nightShiftCharge :: B.C f (Maybe Money),
    oldNightShiftCharge :: B.C f (Maybe Centesimal),
    nightShiftStart :: B.C f (Maybe TimeOfDayText),
    nightShiftEnd :: B.C f (Maybe TimeOfDayText),
    waitingChargePerMin :: B.C f (Maybe Money),
    waitingOrPickupCharges :: B.C f (Maybe Money),
    specialLocationTag :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table EstimateT where
  data PrimaryKey EstimateT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Estimate = EstimateT Identity

$(enableKVPG ''EstimateT ['id] [])

$(mkTableInstancesWithTModifier ''EstimateT "estimate" [("oldNightShiftCharge", "night_shift_multiplier")])
