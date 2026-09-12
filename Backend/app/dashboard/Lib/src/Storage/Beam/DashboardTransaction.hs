{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | lib-dashboard's own view of the shared @transaction@ table.
--
-- A distinct Beam type per owning package, because @FromTType'@ carries a
-- functional dependency @t -> a@: one Beam table determines exactly one domain
-- type. Each owner therefore needs its own Beam type over the same physical
-- table, mapping to its own concrete action enum.
module Storage.Beam.DashboardTransaction where

import qualified Data.Time as Time
import qualified Database.Beam as B
import qualified Domain.Types.ServerName as DSN
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

data DashboardTransactionT f = DashboardTransactionT
  { id :: B.C f Text,
    requestorId :: B.C f (Maybe Text),
    serverName :: B.C f (Maybe DSN.ServerName),
    merchantId :: B.C f (Maybe Text),
    commonDriverId :: B.C f (Maybe Text),
    commonRideId :: B.C f (Maybe Text),
    endpoint :: B.C f Text,
    request :: B.C f (Maybe Text),
    response :: B.C f (Maybe Text),
    responseError :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DashboardTransactionT where
  data PrimaryKey DashboardTransactionT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type DashboardTransaction = DashboardTransactionT Identity

$(enableKVPG ''DashboardTransactionT ['id] [])

$(mkTableInstancesGenericSchema ''DashboardTransactionT "transaction")
