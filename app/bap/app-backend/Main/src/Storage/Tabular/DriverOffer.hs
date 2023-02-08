{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverOffer where

import qualified Domain.Types.DriverOffer as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import qualified Storage.Tabular.Estimate as TEstimate

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverOfferT sql=driver_offer
      id Text
      estimateId TEstimate.EstimateTId
      driverName Text
      durationToPickup Int
      distanceToPickup HighPrecMeters
      validTill UTCTime
      bppQuoteId Text
      rating Centesimal Maybe
      Primary id
      deriving Generic
    |]

instance TEntityKey DriverOfferT where
  type DomainKey DriverOfferT = Id Domain.DriverOffer
  fromKey (DriverOfferTKey _id) = Id _id
  toKey (Id id) = DriverOfferTKey id

instance TType DriverOfferT Domain.DriverOffer where
  fromTType DriverOfferT {..} = do
    return $
      Domain.DriverOffer
        { id = Id id,
          bppQuoteId = Id bppQuoteId,
          estimateId = fromKey estimateId,
          ..
        }
  toTType Domain.DriverOffer {..} = do
    DriverOfferT
      { id = getId id,
        bppQuoteId = bppQuoteId.getId,
        estimateId = toKey estimateId,
        ..
      }
