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

module Storage.Tabular.FarePolicy.CustomerExtraFeeBounds where

import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FarePolicy.CustomerExtraFeeBounds as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (Meters, Money)
import Kernel.Types.Id
import Storage.Tabular.FarePolicy.Table (FarePolicyTId)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    CustomerExtraFeeBoundsT sql=fare_policy_customer_extra_fee_bounds
      Id Int
      farePolicyId FarePolicyTId
      startDistance Meters
      minFee Money
      maxFee Money
      deriving Generic
    |]

type FullCustomerExtraFeeBounds = (Id DFP.FarePolicy, Domain.CustomerExtraFeeBounds)

instance FromTType CustomerExtraFeeBoundsT FullCustomerExtraFeeBounds where
  fromTType CustomerExtraFeeBoundsT {..} = do
    return
      ( fromKey farePolicyId,
        Domain.CustomerExtraFeeBounds
          { ..
          }
      )

instance ToTType CustomerExtraFeeBoundsT FullCustomerExtraFeeBounds where
  toTType (farePolicyId, Domain.CustomerExtraFeeBounds {..}) =
    CustomerExtraFeeBoundsT
      { farePolicyId = toKey farePolicyId,
        ..
      }
