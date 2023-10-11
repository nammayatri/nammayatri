{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.DriverPlan where

import qualified Database.Beam as B
import qualified Domain.Types.Plan as DPlan
import Kernel.Prelude
import Tools.Beam.UtilsTH

data DriverPlanT f = DriverPlanT
  { driverId :: B.C f Text,
    planId :: B.C f Text,
    planType :: B.C f DPlan.PaymentMode,
    mandateId :: B.C f (Maybe Text),
    mandateSetupDate :: B.C f (Maybe UTCTime),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverPlanT where
  data PrimaryKey DriverPlanT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . driverId

type DriverPlan = DriverPlanT Identity

$(enableKVPG ''DriverPlanT ['driverId] [['mandateId]])
$(mkTableInstances ''DriverPlanT "driver_plan")
