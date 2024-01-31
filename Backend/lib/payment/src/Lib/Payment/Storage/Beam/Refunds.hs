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

module Lib.Payment.Storage.Beam.Refunds where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Payment.Interface (RefundStatus)
import Kernel.Prelude
import Kernel.Types.Common hiding (id)

data RefundsT f = RefundsT
  { id :: B.C f Text,
    shortId :: B.C f Text,
    orderId :: B.C f Text,
    refundAmount :: B.C f HighPrecMoney,
    idAssignedByServiceProvider :: B.C f (Maybe Text),
    merchantId :: B.C f Text,
    status :: B.C f RefundStatus,
    errorMessage :: B.C f (Maybe Text),
    errorCode :: B.C f (Maybe Text),
    initiatedBy :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RefundsT where
  data PrimaryKey RefundsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Refunds = RefundsT Identity

$(enableKVPG ''RefundsT ['id] [])

$(mkTableInstancesGenericSchema ''RefundsT "refunds")
