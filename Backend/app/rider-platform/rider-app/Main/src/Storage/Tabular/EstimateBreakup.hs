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

module Storage.Tabular.EstimateBreakup where

import qualified Domain.Types.Estimate as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import qualified Storage.Tabular.Estimate as SEstimate

mkPersist
  defaultSqlSettings
  [defaultQQ|
    EstimateBreakupT sql=estimate_breakup
      id Text
      estimateId SEstimate.EstimateTId
      title Text
      priceCurrency Text
      priceValue HighPrecMoney
      Primary id
      deriving Generic
    |]

instance TEntityKey EstimateBreakupT where
  type DomainKey EstimateBreakupT = Id Domain.EstimateBreakup
  fromKey (EstimateBreakupTKey _id) = Id _id
  toKey (Id id) = EstimateBreakupTKey id

instance FromTType EstimateBreakupT Domain.EstimateBreakup where
  fromTType EstimateBreakupT {..} = do
    return $
      Domain.EstimateBreakup
        { id = Id id,
          price =
            Domain.EstimateBreakupPrice
              { currency = priceCurrency,
                value = roundToIntegral priceValue
              },
          estimateId = fromKey estimateId,
          ..
        }

instance ToTType EstimateBreakupT Domain.EstimateBreakup where
  toTType Domain.EstimateBreakup {..} =
    EstimateBreakupT
      { id = getId id,
        priceCurrency = price.currency,
        priceValue = realToFrac price.value,
        estimateId = toKey estimateId,
        ..
      }
