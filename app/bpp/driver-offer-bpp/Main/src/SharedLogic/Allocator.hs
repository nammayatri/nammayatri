{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module SharedLogic.Allocator where

import Data.Singletons.TH
import qualified Domain.Types.SearchRequest as DSR
import Kernel.Prelude
import Kernel.Types.Common (Meters, Money)
import Kernel.Types.Id
import Kernel.Utils.Dhall (FromDhall)
import Lib.Scheduler

data AllocatorJobType = SendSearchRequestToDriver
  deriving (Generic, FromDhall, Eq, Ord, Show, Read, FromJSON, ToJSON)

genSingletons [''AllocatorJobType]
singEqInstances [''AllocatorJobType]
singOrdInstances [''AllocatorJobType]
showSingInstances [''AllocatorJobType]

data SendSearchRequestToDriverJobData = SendSearchRequestToDriverJobData
  { requestId :: Id DSR.SearchRequest,
    baseFare :: Money,
    estimatedRideDistance :: Meters,
    driverMinExtraFee :: Money,
    driverMaxExtraFee :: Money
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

type instance JobContent 'SendSearchRequestToDriver = SendSearchRequestToDriverJobData
