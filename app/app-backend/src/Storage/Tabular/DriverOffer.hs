{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverOffer where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Types.DriverOffer as Domain
import qualified Storage.Tabular.Estimate as TEstimate

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverOfferT sql=driver_offer
      id Text
      estimateId TEstimate.EstimateTId
      driverName Text
      durationToPickup Int
      distanceToPickup Double
      validTill UTCTime
      bppQuoteId Text
      rating Double Maybe
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
          distanceToPickup = HighPrecMeters distanceToPickup,
          ..
        }
  toTType Domain.DriverOffer {..} = do
    DriverOfferT
      { id = getId id,
        bppQuoteId = bppQuoteId.getId,
        estimateId = toKey estimateId,
        distanceToPickup = getHighPrecMeters distanceToPickup,
        ..
      }
