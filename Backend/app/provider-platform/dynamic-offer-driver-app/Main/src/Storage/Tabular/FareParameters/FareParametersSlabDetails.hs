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
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FareParameters.FareParametersSlabDetails where

import qualified Domain.Types.FareParameters as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.FareParameters.Table (FareParametersTId)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FareParametersSlabDetailsT sql=fare_parameters_slab_details
      fareParametersId FareParametersTId
      platformFee Money Maybe
      sgst HighPrecMoney Maybe
      cgst HighPrecMoney Maybe

      Primary fareParametersId
      deriving Generic
    |]

instance TEntityKey FareParametersSlabDetailsT where
  type DomainKey FareParametersSlabDetailsT = Id Domain.FareParameters
  fromKey (FareParametersSlabDetailsTKey _id) = fromKey _id
  toKey id = FareParametersSlabDetailsTKey $ toKey id

type FullFareParametersSlabDetails = (Id Domain.FareParameters, Domain.FParamsSlabDetails)

instance FromTType FareParametersSlabDetailsT FullFareParametersSlabDetails where
  fromTType FareParametersSlabDetailsT {..} = do
    return
      ( fromKey fareParametersId,
        Domain.FParamsSlabDetails
          { ..
          }
      )

instance ToTType FareParametersSlabDetailsT FullFareParametersSlabDetails where
  toTType (fareParametersId, Domain.FParamsSlabDetails {..}) = do
    FareParametersSlabDetailsT
      { fareParametersId = toKey fareParametersId,
        ..
      }
