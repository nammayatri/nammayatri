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

module Storage.Tabular.SearchStep where

import qualified Domain.Types.SearchStep as Domain
import qualified Domain.Types.Vehicle.Variant as Variant (Variant)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Estimate (EstimateTId)
import Storage.Tabular.SearchRequest (SearchRequestTId)
import Storage.Tabular.Vehicle ()

derivePersistField "Domain.SearchStepStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchStepT sql=search_step
      id Text
      requestId SearchRequestTId
      estimateId EstimateTId
      messageId Text
      startTime UTCTime
      validTill UTCTime
      status Domain.SearchStepStatus
      searchRepeatCounter Int
      vehicleVariant Variant.Variant
      createdAt UTCTime
      updatedAt UTCTime

      Primary id
      deriving Generic
    |]

instance TEntityKey SearchStepT where
  type DomainKey SearchStepT = Id Domain.SearchStep
  fromKey (SearchStepTKey _id) = Id _id
  toKey (Id id) = SearchStepTKey id

instance FromTType SearchStepT Domain.SearchStep where
  fromTType SearchStepT {..} = do
    return $
      Domain.SearchStep
        { id = Id id,
          requestId = fromKey requestId,
          estimateId = fromKey estimateId,
          ..
        }

instance ToTType SearchStepT Domain.SearchStep where
  toTType Domain.SearchStep {..} =
    SearchStepT
      { id = getId id,
        requestId = toKey requestId,
        estimateId = toKey estimateId,
        ..
      }
