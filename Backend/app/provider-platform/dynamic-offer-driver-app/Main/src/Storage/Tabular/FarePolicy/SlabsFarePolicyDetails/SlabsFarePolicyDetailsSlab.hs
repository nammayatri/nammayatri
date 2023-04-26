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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FarePolicy.SlabsFarePolicyDetails.SlabsFarePolicyDetailsSlab where

import qualified Domain.Types.FarePolicy as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (HighPrecMoney, Meters)
import Kernel.Types.Id
import Storage.Tabular.FarePolicy (FarePolicyTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SlabsFarePolicyDetailsSlabT sql=fare_policy_slabs_details_slab
      farePolicyId FarePolicyTId
      startMeters Meters
      baseFare HighPrecMoney
      waitingCharge Domain.WaitingCharge Maybe
      nightShiftChargeType Text Maybe
      nightShiftCharge Domain.NightShiftCharge Maybe

      Primary farePolicyId
      deriving Generic
    |]

instance TEntityKey SlabsFarePolicyDetailsSlabT where
  type DomainKey SlabsFarePolicyDetailsSlabT = Id Domain.FarePolicy
  fromKey (SlabsFarePolicyDetailsSlabTKey _id) = fromKey _id
  toKey id = SlabsFarePolicyDetailsSlabTKey $ toKey id
