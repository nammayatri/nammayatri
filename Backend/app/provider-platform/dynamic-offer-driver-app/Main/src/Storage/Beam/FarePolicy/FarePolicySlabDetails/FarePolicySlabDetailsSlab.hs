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

module Storage.Beam.FarePolicy.FarePolicySlabDetails.FarePolicySlabDetailsSlab where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as DPM
import qualified Database.Beam as B
import Database.Beam.Backend ()
import qualified Domain.Types.FarePolicy as Domain
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import qualified Kernel.Types.Id as KTI
import Tools.Beam.UtilsTH

data FarePolicySlabsDetailsSlabT f = FarePolicySlabsDetailsSlabT
  { id :: B.C f (Maybe Int),
    farePolicyId :: B.C f Text,
    startDistance :: B.C f Meters,
    baseFare :: B.C f Money,
    platformFeeCharge :: B.C f (Maybe Domain.PlatformFeeCharge),
    platformFeeCgst :: B.C f (Maybe Double),
    platformFeeSgst :: B.C f (Maybe Double),
    waitingCharge :: B.C f (Maybe DPM.WaitingCharge),
    freeWatingTime :: B.C f (Maybe Minutes), -- FIXME typo
    nightShiftCharge :: B.C f (Maybe DPM.NightShiftCharge)
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicySlabsDetailsSlabT where
  data PrimaryKey FarePolicySlabsDetailsSlabT f
    = Id (B.C f (Maybe Int))
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type FarePolicySlabsDetailsSlab = FarePolicySlabsDetailsSlabT Identity

type FullFarePolicySlabsDetailsSlab = (KTI.Id Domain.FarePolicy, Domain.FPSlabsDetailsSlab)

$(enableKVPG ''FarePolicySlabsDetailsSlabT ['id] [['farePolicyId]])

$(mkTableInstances ''FarePolicySlabsDetailsSlabT "fare_policy_slabs_details_slab")
