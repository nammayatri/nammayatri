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

instance TType EstimateBreakupT Domain.EstimateBreakup where
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
  toTType Domain.EstimateBreakup {..} =
    EstimateBreakupT
      { id = getId id,
        priceCurrency = price.currency,
        priceValue = realToFrac price.value,
        estimateId = toKey estimateId,
        ..
      }
