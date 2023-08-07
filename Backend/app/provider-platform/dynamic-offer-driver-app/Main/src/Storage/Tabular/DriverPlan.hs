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
import qualified Domain.Types.PlanDetails as PD
import Domain.Types.Person (Person)
import qualified Domain.Types.PlanDetails as DPlan
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.PlanDetails ()

derivePersistField "Domain.MandateStatus"
derivePersistField "Domain.PlanStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverPlanT sql=driver_plan
      driverId PersonTId
      planId PlanDetailsTId
      planType PD.PaymentMode
      mandateId Text Maybe
      mandateStatus Domain.MandateStatus Maybe
      planStatus Domain.PlanStatus
      activatedAt UTCTime Maybe
      endAt UTCTime Maybe
      resumeDate UTCTime Maybe
      maxAmount Int
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey DriverPlanT where
  type DomainKey DriverPlanT = Id Person
  fromKey (DriverPlanTKey _driverId) = fromKey _driverId
  toKey driverId = DriverPlanTKey $ toKey driverId

instance FromTType DriverPlanT Domain.DriverPlan where
  fromTType DriverPlanT {..} = do
    return $
      Domain.DriverPlan
        { driverId = fromKey driverId,
          planId = Id planId,
          ..
        }

instance ToTType DriverPlanT Domain.DriverPlan where
  toTType Domain.DriverPlan {..} = do
    DriverPlanT
      { driverId = toKey driverId,
        planId = getId planId,
        ..
      }
