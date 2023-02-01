{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module SharedLogic.Scheduler where

import Data.Singletons.TH
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Merchant as DM
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.GenericPretty
import Lib.Scheduler

data SchedulerJobType
  = AllocateRental
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

genSingletons [''SchedulerJobType]
singEqInstances [''SchedulerJobType]
showSingInstances [''SchedulerJobType]

data AllocateRentalJobData = AllocateRentalJobData
  { bookingId :: Id DRB.Booking,
    shortOrgId :: ShortId DM.Subscriber
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, PrettyShow)

instance JobProcessor SchedulerJobType

type instance JobContent 'AllocateRental = AllocateRentalJobData

instance {-# OVERLAPS #-} JobInfoProcessor 'AllocateRental
