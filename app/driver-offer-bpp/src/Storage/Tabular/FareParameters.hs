{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Tabular.FareParameters where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.FareParams.Internal as Domain
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FareParametersT sql=fare_parameters

      id Text
      baseFare Amount
      extraKmFare Amount Maybe
      driverSelectedFare Amount Maybe
      nightShiftRate Amount Maybe
      nightCoefIncluded Bool

      Primary id
      deriving Generic
    |]

instance TEntityKey FareParametersT where
  type DomainKey FareParametersT = Id Domain.FareParameters
  fromKey (FareParametersTKey _id) = Id _id
  toKey (Id id) = FareParametersTKey id

mkDomainFromTabularFareParams :: FareParametersT -> Domain.FareParameters
mkDomainFromTabularFareParams FareParametersT {..} = Domain.FareParameters {..}

mkTabularFromDomainFareParams :: Id Domain.FareParameters -> Domain.FareParameters -> FareParametersT
mkTabularFromDomainFareParams id Domain.FareParameters {..} = FareParametersT {id = id.getId, ..}
