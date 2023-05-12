{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FareParameters.Instances where

import qualified Domain.Types.FareParameters as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.FareParameters.FareParametersProgressiveDetails
import Storage.Tabular.FareParameters.Table
import Storage.Tabular.Vehicle ()
import Tools.Error

type FullFareParametersT = (FareParametersT, FareParametersDetailsT)

data FareParametersDetailsT = ProgressiveDetailsT FareParametersProgressiveDetailsT | SlabDetailsT

instance FromTType FullFareParametersT Domain.FareParameters where
  fromTType (FareParametersT {..}, fareParametersDetails) = do
    det <- case fareParametersDetails of
      ProgressiveDetailsT detT -> do
        (fareParametersId, det) <- fromTType @_ @FullFareParametersProgressiveDetails detT
        unless (fareParametersId == Id id) $ throwError (InternalError "Unable to decode progressive FareParameters. Fare parameters ids are not the same.")
        return $ Domain.ProgressiveDetails det
      SlabDetailsT -> return $ Domain.SlabDetails Domain.FParamsSlabDetails
    return $
      Domain.FareParameters
        { id = Id id,
          fareParametersDetails = det,
          ..
        }

instance ToTType FullFareParametersT Domain.FareParameters where
  toTType fareParameters@Domain.FareParameters {..} = do
    let detT = case fareParametersDetails of
          Domain.ProgressiveDetails det -> ProgressiveDetailsT $ toTType (id, det)
          Domain.SlabDetails _ -> SlabDetailsT
    ( FareParametersT
        { id = getId id,
          fareParametersType = Domain.getFareParametersType fareParameters,
          ..
        },
      detT
      )
