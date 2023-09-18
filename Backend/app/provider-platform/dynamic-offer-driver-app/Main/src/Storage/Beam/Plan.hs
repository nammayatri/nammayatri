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

module Storage.Beam.Plan where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Plan as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize
import Tools.Beam.UtilsTH

data PlanT f = PlanT
  { id :: B.C f Text,
    paymentMode :: B.C f Domain.PaymentMode,
    merchantId :: B.C f Text,
    name :: B.C f Text,
    description :: B.C f Text,
    maxAmount :: B.C f HighPrecMoney,
    registrationAmount :: B.C f HighPrecMoney,
    isOfferApplicable :: B.C f Bool,
    maxCreditLimit :: B.C f HighPrecMoney,
    planBaseAmount :: B.C f Domain.PlanBaseAmount,
    freeRideCount :: B.C f Int,
    frequency :: B.C f Domain.Frequency,
    planType :: B.C f Domain.PlanType,
    cgstPercentage :: B.C f HighPrecMoney,
    sgstPercentage :: B.C f HighPrecMoney
  }
  deriving (Generic, B.Beamable)

instance B.Table PlanT where
  data PrimaryKey PlanT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Plan = PlanT Identity

$(enableKVPG ''PlanT ['id] [['paymentMode], ['merchantId]]) -- DON'T Enable for KV

$(mkTableInstances ''PlanT "plan")
