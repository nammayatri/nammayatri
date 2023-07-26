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

module Storage.Tabular.DriverPlan where

import qualified Domain.Types.DriverPlan as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (Money)
import Kernel.Types.Id
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.PlanDetails (PlanDetailsTId)

derivePersistField "Domain.PaymentMethodType"
derivePersistField "Domain.PaymentMethod"
derivePersistField "Domain.MandateStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverPlanT sql=driver_plan
      id Text
      driverId PersonTId
      planId PlanDetailsTId
      maxAmount Money
      paymentMethodType Domain.PaymentMethodType
      paymentMethod Domain.PaymentMethod
      mandateStatus Domain.MandateStatus
      activatedAt UTCTime
      autoPay Bool
      Primary id
      deriving Generic
    |]

instance TEntityKey DriverPlanT where
  type DomainKey DriverPlanT = Id Domain.DriverPlan
  fromKey (DriverPlanTKey _id) = Id _id
  toKey (Id id) = DriverPlanTKey id

instance FromTType DriverPlanT Domain.DriverPlan where
  fromTType DriverPlanT {..} = do
    return $
      Domain.DriverPlan
        { id = Id id,
          driverId = fromKey driverId,
          planId = fromKey planId,
          ..
        }

instance ToTType DriverPlanT Domain.DriverPlan where
  toTType Domain.DriverPlan {..} = do
    DriverPlanT
      { id = getId id,
        driverId = toKey driverId,
        planId = toKey planId,
        ..
      }
