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

module Storage.Beam.MerchantAccess where

import qualified Data.Time as Time
import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.Beckn.City (City)

data MerchantAccessT f = MerchantAccessT
  { id :: B.C f Text,
    personId :: B.C f Text,
    merchantId :: B.C f Text,
    merchantShortId :: B.C f Text,
    operatingCity :: B.C f City,
    secretKey :: B.C f (Maybe Text),
    is2faEnabled :: B.C f Bool,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantAccessT where
  data PrimaryKey MerchantAccessT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type MerchantAccess = MerchantAccessT Identity

$(enableKVPG ''MerchantAccessT ['id] [])

$(mkTableInstancesGenericSchema ''MerchantAccessT "merchant_access")
