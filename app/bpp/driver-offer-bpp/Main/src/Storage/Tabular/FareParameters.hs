{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FareParameters where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common (Centesimal, Money)
import Beckn.Types.Id
import qualified Domain.Types.FareParameters as Domain
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

mkDomainFromTabularFareParams :: FareParametersT -> Domain.FareParameters
mkDomainFromTabularFareParams FareParametersT {..} =
  Domain.FareParameters
    { ..
    }

mkTabularFromDomainFareParams :: Id Domain.FareParameters -> Domain.FareParameters -> FareParametersT
mkTabularFromDomainFareParams id Domain.FareParameters {..} =
  FareParametersT
    { id = id.getId,
      ..
    }

instance TType FareParametersT Domain.FareParameters' where
  fromTType FareParametersT {..} = do
    return $
      Domain.FareParameters'
        { id = Id id,
          ..
        }
  toTType Domain.FareParameters' {..} = do
    FareParametersT
      { id = getId id,
        ..
      }
