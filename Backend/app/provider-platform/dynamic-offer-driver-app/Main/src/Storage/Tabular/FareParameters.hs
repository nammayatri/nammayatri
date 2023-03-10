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
      deadKmFare Money Maybe
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

instance FromTType FareParametersT Domain.FareParameters where
  fromTType FareParametersT {..} = do
    return $
      Domain.FareParameters
        { id = Id id,
          ..
        }

instance ToTType FareParametersT Domain.FareParameters where
  toTType Domain.FareParameters {..} = do
    FareParametersT
      { id = getId id,
        ..
      }
