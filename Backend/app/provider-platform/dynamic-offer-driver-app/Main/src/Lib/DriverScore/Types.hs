{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Lib.DriverScore.Types
  ( DriverRideRequest (..),
  )
where

import Data.Time
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequestForDriver as SRD
import qualified Domain.Types.SearchTry as DST
import EulerHS.Prelude hiding (Show)
import Kernel.Prelude (Show)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id (Id)
import qualified SharedLogic.DriverPool as DP

data DriverRideRequest
  = OnDriverAcceptingSearchRequest
      { merchantId :: Id DM.Merchant,
        driverId :: Id DP.Person,
        searchTryId :: Id DST.SearchTry,
        restDriverIds :: [Id DP.Person],
        response :: SRD.SearchRequestForDriverResponse
      }
  | OnNewRideAssigned
      { merchantId :: Id DM.Merchant,
        driverId :: Id DP.Person
      }
  | OnNewSearchRequestForDrivers
      { driverPool :: [DP.DriverPoolWithActualDistResult],
        merchantId :: Id DM.Merchant,
        searchReq :: DSR.SearchRequest,
        searchTry :: DST.SearchTry,
        validTill :: UTCTime,
        batchProcessTime :: Redis.ExpirationTime
      }
  | OnDriverCancellation
      { merchantId :: Id DM.Merchant,
        driverId :: Id DP.Person,
        rideFare :: Maybe Money
      }
  | OnRideCompletion
      { merchantId :: Id DM.Merchant,
        driverId :: Id DP.Person,
        ride :: DR.Ride
      }
  deriving (Show)
