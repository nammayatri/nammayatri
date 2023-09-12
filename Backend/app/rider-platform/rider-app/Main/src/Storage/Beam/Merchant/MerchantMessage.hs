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

module Storage.Beam.Merchant.MerchantMessage where

import qualified Database.Beam as B
import qualified Domain.Types.Merchant.MerchantMessage as Domain
import Kernel.Prelude
import Tools.Beam.UtilsTH

data MerchantMessageT f = MerchantMessageT
  { merchantId :: B.C f Text,
    messageKey :: B.C f Domain.MessageKey,
    message :: B.C f Text,
    updatedAt :: B.C f UTCTime,
    createdAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantMessageT where
  data PrimaryKey MerchantMessageT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantId

type MerchantMessage = MerchantMessageT Identity

$(enableKVPG ''MerchantMessageT ['merchantId, 'messageKey] [])

$(mkTableInstances ''MerchantMessageT "merchant_message")
