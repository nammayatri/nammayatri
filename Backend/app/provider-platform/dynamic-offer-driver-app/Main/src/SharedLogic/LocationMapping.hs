{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.LocationMapping where

import qualified Data.Time.Clock.POSIX as Time
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Storage.Queries.LocationMapping as QLM

buildPickUpLocationMapping :: MonadFlow m => Id DL.Location -> Text -> DLM.LocationMappingTags -> m DLM.LocationMapping
buildPickUpLocationMapping locationId entityId tag = do
  version <- getMappingVersion
  id <- generateGUID
  let order = 0
  return DLM.LocationMapping {..}

buildDropLocationMapping :: MonadFlow m => Id DL.Location -> Text -> DLM.LocationMappingTags -> m DLM.LocationMapping
buildDropLocationMapping locationId entityId tag = do
  id <- generateGUID
  version <- getMappingVersion
  noOfEntries <- QLM.countOrders entityId
  let order = if noOfEntries == 0 then 1 else noOfEntries
  return DLM.LocationMapping {..}

getMappingVersion :: MonadFlow m => m Text
getMappingVersion = do
  now <- getCurrentTime
  let epochVersion = round $ Time.utcTimeToPOSIXSeconds now
  let epochVersionLast5Digits = epochVersion `mod` 100000 :: Integer
  pure $ show epochVersionLast5Digits
