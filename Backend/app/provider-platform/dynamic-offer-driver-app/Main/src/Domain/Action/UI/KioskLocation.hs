{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.KioskLocation
  ( listKioskLocations,
    KioskLocationRes,
  )
where

import qualified Domain.Types.KioskLocation as DTK
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified Storage.CachedQueries.KioskLocation as CQK
import qualified Storage.CachedQueries.KioskLocationTranslation as CQKT
import qualified Storage.Queries.Person as QPerson
import Tools.Error

type KioskLocationRes = [DTK.KioskLocation]

listKioskLocations :: (Id Person.Person, Id DM.Merchant) -> Flow KioskLocationRes
listKioskLocations (personId, mId) = do
  locations <- CQK.fetchAllKioskLocationsByMerchant mId
  driver <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let language = fromMaybe ENGLISH driver.language
  mapM (translateKioskLocation language) locations
  where
    translateKioskLocation language location@DTK.KioskLocation {..} = do
      translation <- CQKT.findByKioskLocationIdAndLanguage location.id language
      return $
        DTK.KioskLocation
          { address = maybe location.address (.address) translation,
            landmark = maybe location.landmark (.landmark) translation,
            ..
          }
