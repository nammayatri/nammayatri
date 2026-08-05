{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.MerchantPair where

import qualified Data.Time as Time
import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

data MerchantPairT f = MerchantPairT
  { logicalShortId :: B.C f Text,
    bapMerchantId :: B.C f (Maybe Text),
    bppMerchantId :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantPairT where
  data PrimaryKey MerchantPairT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . logicalShortId

type MerchantPair = MerchantPairT Identity

$(enableKVPG ''MerchantPairT ['logicalShortId] [])

$(mkTableInstancesGenericSchema ''MerchantPairT "merchant_pair")
