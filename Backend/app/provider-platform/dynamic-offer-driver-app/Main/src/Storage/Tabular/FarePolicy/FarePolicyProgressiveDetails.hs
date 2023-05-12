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

module Storage.Tabular.FarePolicy.FarePolicyProgressiveDetails where

import qualified Domain.Types.FarePolicy as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (HighPrecMoney, Meters, Money)
import Kernel.Types.Id
import Storage.Tabular.FarePolicy.Table (FarePolicyTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FarePolicyProgressiveDetailsT sql=fare_policy_progressive_details
      farePolicyId FarePolicyTId
      baseDistance Meters
      baseFare Money
      perExtraKmFare HighPrecMoney
      deadKmFare Money
      waitingCharge Domain.WaitingCharge Maybe
      nightShiftCharge Domain.NightShiftCharge Maybe

      Primary farePolicyId
      deriving Generic
    |]

instance TEntityKey FarePolicyProgressiveDetailsT where
  type DomainKey FarePolicyProgressiveDetailsT = Id Domain.FarePolicy
  fromKey (FarePolicyProgressiveDetailsTKey _id) = fromKey _id
  toKey id = FarePolicyProgressiveDetailsTKey $ toKey id

type FullFarePolicyProgressiveDetails = (Id Domain.FarePolicy, Domain.FPProgressiveDetails)

instance FromTType FarePolicyProgressiveDetailsT FullFarePolicyProgressiveDetails where
  fromTType FarePolicyProgressiveDetailsT {..} = do
    return
      ( fromKey farePolicyId,
        Domain.FPProgressiveDetails
          { ..
          }
      )

instance ToTType FarePolicyProgressiveDetailsT FullFarePolicyProgressiveDetails where
  toTType (farePolicyId, Domain.FPProgressiveDetails {..}) = do
    FarePolicyProgressiveDetailsT
      { farePolicyId = toKey farePolicyId,
        ..
      }
