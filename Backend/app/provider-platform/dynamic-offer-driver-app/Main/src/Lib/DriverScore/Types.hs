{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Lib.DriverScore.Types
  ( DriverRideRequeset (..),
  )
where

import Data.Time
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SearchRequest as SR
import qualified Domain.Types.SearchRequestForDriver as SRD
import Kernel.Prelude (Show)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id (Id)
import qualified SharedLogic.DriverPool as DP

data DriverRideRequeset
  = OnDriverAcceptingSearchRequest
      { merchantId :: Id DM.Merchant,
        driverId :: Id DP.Person,
        searchReqId :: Id SR.SearchRequest,
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
        searchReq :: SR.SearchRequest,
        validTill :: UTCTime,
        batchProcessTime :: Redis.ExpirationTime
      }
  | OnDriverCancellation
      { merchantId :: Id DM.Merchant,
        driverId :: Id DP.Person
      }
  deriving (Show)
