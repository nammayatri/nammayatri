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

module Storage.Beam.Person.PersonDefaultEmergencyNumber where

import qualified Database.Beam as B
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude
import qualified Tools.Beam.UtilsTH as TH

data PersonDefaultEmergencyNumberT f = PersonDefaultEmergencyNumberT
  { personId :: B.C f Text,
    name :: B.C f Text,
    mobileCountryCode :: B.C f Text,
    mobileNumberEncrypted :: B.C f Text,
    mobileNumberHash :: B.C f DbHash,
    createdAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonDefaultEmergencyNumberT where
  data PrimaryKey PersonDefaultEmergencyNumberT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . personId

type PersonDefaultEmergencyNumber = PersonDefaultEmergencyNumberT Identity

$(TH.enableKVPG ''PersonDefaultEmergencyNumberT ['personId] [])

$(TH.mkTableInstances ''PersonDefaultEmergencyNumberT "person_default_emergency_number")
