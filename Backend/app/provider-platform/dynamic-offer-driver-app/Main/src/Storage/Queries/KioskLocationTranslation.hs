{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.KioskLocationTranslation where

import qualified Domain.Types.KioskLocation as KioskLocation
import Domain.Types.KioskLocationTranslation
import Kernel.Beam.Functions
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Sequelize as Se
import qualified Storage.Beam.KioskLocationTranslation as BeamKT

create :: MonadFlow m => KioskLocationTranslation -> m ()
create = createWithKV

findByKioskLocationIdAndLanguage :: MonadFlow m => Id KioskLocation.KioskLocation -> Language -> m (Maybe KioskLocationTranslation)
findByKioskLocationIdAndLanguage (Id kioskLocationId) language = findOneWithKV [Se.And [Se.Is BeamKT.kioskLocationId $ Se.Eq kioskLocationId, Se.Is BeamKT.language $ Se.Eq language]]

instance FromTType' BeamKT.KioskLocationTranslation KioskLocationTranslation where
  fromTType' BeamKT.KioskLocationTranslationT {..} = do
    pure $
      Just
        KioskLocationTranslation
          { kioskLocationId = Id kioskLocationId,
            language = language,
            landmark = landmark,
            address = address
          }

instance ToTType' BeamKT.KioskLocationTranslation KioskLocationTranslation where
  toTType' KioskLocationTranslation {..} = do
    BeamKT.KioskLocationTranslationT
      { BeamKT.kioskLocationId = getId kioskLocationId,
        BeamKT.language = language,
        BeamKT.landmark = landmark,
        BeamKT.address = address
      }
