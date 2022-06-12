{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RentalSlab where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Types.RentalSlab as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RentalSlabT sql=rental_slab
      id Text
      baseDistance Int
      baseDuration Int
      Primary id
      deriving Generic
    |]

instance TEntityKey RentalSlabT where
  type DomainKey RentalSlabT = Id Domain.RentalSlab
  fromKey (RentalSlabTKey _id) = Id _id
  toKey (Id id) = RentalSlabTKey id

instance TType RentalSlabT Domain.RentalSlab where
  fromTType RentalSlabT {..} =
    return $
      Domain.RentalSlab
        { id = Id id,
          baseDistance = Kilometers baseDistance,
          baseDuration = Hours baseDuration
        }
  toTType Domain.RentalSlab {..} =
    RentalSlabT
      { id = getId id,
        baseDistance = getKilometers baseDistance,
        baseDuration = getHours baseDuration
      }
