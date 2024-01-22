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

module Storage.Beam.Transaction where

import qualified Data.Time as Time
import qualified Database.Beam as B
import qualified Domain.Types.ServerName as DSN
import qualified Domain.Types.Transaction as Domain
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

data TransactionT f = TransactionT
  { id :: B.C f Text,
    requestorId :: B.C f (Maybe Text),
    serverName :: B.C f (Maybe DSN.ServerName),
    merchantId :: B.C f (Maybe Text),
    commonDriverId :: B.C f (Maybe Text),
    commonRideId :: B.C f (Maybe Text),
    endpoint :: B.C f Domain.Endpoint,
    request :: B.C f (Maybe Text),
    response :: B.C f (Maybe Text),
    responseError :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table TransactionT where
  data PrimaryKey TransactionT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Transaction = TransactionT Identity

$(enableKVPG ''TransactionT ['id] [])

$(mkTableInstancesGenericSchema ''TransactionT "transaction")
