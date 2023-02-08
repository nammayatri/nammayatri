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
singOrdInstances [''SchedulerJobType]
showSingInstances [''SchedulerJobType]

data AllocateRentalJobData = AllocateRentalJobData
  { bookingId :: Id DRB.Booking,
    shortOrgId :: ShortId DM.Subscriber
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, PrettyShow)

type instance JobContent 'AllocateRental = AllocateRentalJobData
