{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DisabilityType where

import Database.Beam.Postgres (Postgres)
import Domain.Types.DisabilityType
import Kernel.Beam.Functions
import Kernel.External.Types
import Kernel.Prelude
import Kernel.Utils.Common (MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.DisabilityType as BeamD

findAllDisabilityTypeWithSeCondition :: MonadFlow m => [Se.Clause Postgres BeamD.DisabilityTypeT] -> m [DisabilityType]
findAllDisabilityTypeWithSeCondition = findAllWithKV

findAllByLanguage :: MonadFlow m => Language -> m [DisabilityType]
findAllByLanguage language = findAllDisabilityTypeWithSeCondition [Se.Is BeamD.language $ Se.Eq language]

instance FromTType' BeamD.DisabilityType DisabilityType where
  fromTType' BeamD.DisabilityTypeT {..} = do
    pure $
      Just
        DisabilityType
          { tag = tag,
            language = language,
            onBookingMessage = onBookingMessage,
            onArrivalMessage = onArrivalMessage,
            onRideStartMessage = onRideStartMessage
          }

instance ToTType' BeamD.DisabilityType DisabilityType where
  toTType' DisabilityType {..} = do
    BeamD.DisabilityTypeT
      { BeamD.tag = tag,
        BeamD.language = language,
        BeamD.onBookingMessage = onBookingMessage,
        BeamD.onArrivalMessage = onArrivalMessage,
        BeamD.onRideStartMessage = onRideStartMessage
      }
