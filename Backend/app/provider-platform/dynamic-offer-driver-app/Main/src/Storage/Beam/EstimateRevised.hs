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
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.EstimateRevised where

import qualified Database.Beam as B
import Domain.Types.Common (TripCategory (..))
-- import qualified Domain.Types.EstimateRevised as Domain
-- import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.Vehicle as Variant
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data EstimateRevisedT f = EstimateRevisedT
  { id :: B.C f Text,
    requestId :: B.C f Text,
    vehicleVariant :: B.C f Variant.Variant,
    tripCategory :: B.C f (Maybe TripCategory),
    minFare :: B.C f Money,
    maxFare :: B.C f Money,
    estimatedDistance :: B.C f (Maybe Meters),
    fareParamsId :: B.C f (Maybe Text),
    farePolicyId :: B.C f (Maybe Text),
    specialLocationTag :: B.C f (Maybe Text),
    isScheduled :: B.C f (Maybe Bool),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f (Maybe UTCTime)
    -- convienceFee :: B.C f Money
    -- parentEstimateId :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table EstimateRevisedT where
  data PrimaryKey EstimateRevisedT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type EstimateRevised = EstimateRevisedT Identity

$(enableKVPG ''EstimateRevisedT ['id] [])

$(mkTableInstancesWithTModifier ''EstimateRevisedT "estimate_revised" [])
