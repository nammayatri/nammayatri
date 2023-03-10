{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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

instance JobProcessor AllocatorJobType

genSingletons [''AllocatorJobType]
showSingInstance ''AllocatorJobType

data SendSearchRequestToDriverJobData = SendSearchRequestToDriverJobData
  { requestId :: Id DSR.SearchRequest,
    baseFare :: Money,
    estimatedRideDistance :: Meters,
    driverMinExtraFee :: Money,
    driverMaxExtraFee :: Money
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'SendSearchRequestToDriver

type instance JobContent 'SendSearchRequestToDriver = SendSearchRequestToDriverJobData
