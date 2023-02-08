{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FareParameters where

import qualified Domain.Types.FareParameters as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (Centesimal, Money)
import Kernel.Types.Id
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FareParametersT sql=fare_parameters

      id Text
      baseFare Money
      extraKmFare Money Maybe
      driverSelectedFare Money Maybe
      nightShiftRate Centesimal Maybe
      nightCoefIncluded Bool
      waitingChargePerMin Money Maybe

      Primary id
      deriving Generic
    |]

instance TEntityKey FareParametersT where
  type DomainKey FareParametersT = Id Domain.FareParameters
  fromKey (FareParametersTKey _id) = Id _id
  toKey (Id id) = FareParametersTKey id

instance TType FareParametersT Domain.FareParameters where
  fromTType FareParametersT {..} = do
    return $
      Domain.FareParameters
        { id = Id id,
          ..
        }
  toTType Domain.FareParameters {..} = do
    FareParametersT
      { id = getId id,
        ..
      }
