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

module Storage.Beam.Driver.GoHomeFeature.DriverHomeLocation where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import qualified Data.Time as Time
import qualified Database.Beam as B
import Kernel.Prelude
import Tools.Beam.UtilsTH

data DriverHomeLocationT f = DriverHomeLocationT
  { id :: B.C f Text,
    driverId :: B.C f Text,
    lat :: B.C f Double,
    lon :: B.C f Double,
    address :: B.C f Text,
    tag :: B.C f Text,
    updatedAt :: B.C f Time.UTCTime,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverHomeLocationT where
  data PrimaryKey DriverHomeLocationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type DriverHomeLocation = DriverHomeLocationT Identity

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverHomeLocationToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverHomeLocationToHSModifiers =
  M.empty

driverHomeLocationToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverHomeLocationToPSModifiers =
  M.empty

$(enableKVPG ''DriverHomeLocationT ['id] [])

$(mkTableInstancesWithTModifier ''DriverHomeLocationT "driver_home_location" [("address", "home_address")])
