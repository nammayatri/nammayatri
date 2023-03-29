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

module Storage.Tabular.Estimate where

import qualified Domain.Types.Estimate as Domain
import qualified Domain.Types.VehicleVariant as VehVar
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common hiding (id)
import qualified Kernel.Types.CommonImport as Domain
import Kernel.Types.Id
import qualified Storage.Tabular.SearchRequest as SSearchRequest
import qualified Storage.Tabular.TripTerms as STripTerms

derivePersistField "Domain.LatLong"
derivePersistField "Domain.EstimateStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    EstimateT sql=estimate
      id Text
      requestId SSearchRequest.SearchRequestTId
      estimatedFare HighPrecMoney
      discount HighPrecMoney Maybe
      estimatedTotalFare HighPrecMoney
      minTotalFare HighPrecMoney
      maxTotalFare HighPrecMoney
      providerId Text
      providerUrl Text
      providerName Text
      providerMobileNumber Text
      providerCompletedRidesCount Int
      vehicleVariant VehVar.VehicleVariant
      driversLocation (PostgresList Domain.LatLong)
      tripTermsId STripTerms.TripTermsTId Maybe
      nightShiftMultiplier Centesimal Maybe
      nightShiftStart TimeOfDay Maybe
      nightShiftEnd TimeOfDay Maybe
      status Domain.EstimateStatus Maybe
      waitingChargePerMin Money Maybe
      waitingTimeEstimatedThreshold Seconds Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey EstimateT where
  type DomainKey EstimateT = Id Domain.Estimate
  fromKey (EstimateTKey _id) = Id _id
  toKey (Id id) = EstimateTKey id
