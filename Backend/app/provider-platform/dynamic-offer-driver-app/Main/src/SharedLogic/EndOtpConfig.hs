{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.EndOtpConfig (checkEndOtpRequired) where

import qualified Domain.Types as DTC
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Storage.CachedQueries.EndOtpConfig as CQEndOtpConfig

-- Per-city, per-(category,mode) override of DTC.isEndOtpRequired, driven by the
-- EndOtpConfig table. Falls back to the old hardcoded pure function whenever no row is
-- found (unconfigured city, or a (category, mode) combination the seed migration missed),
-- so behaviour is unchanged unless someone explicitly configures a row.
checkEndOtpRequired :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id MerchantOperatingCity -> DTC.TripCategory -> m Bool
checkEndOtpRequired merchantOperatingCityId tripCategory = do
  let (tripCategoryText, tripModeText) = DTC.tripCategoryAndModeText tripCategory
  mbConfig <- CQEndOtpConfig.findByMerchantOpCityIdAndTripCategoryAndTripMode merchantOperatingCityId tripCategoryText tripModeText
  case mbConfig of
    Just config -> pure config.isEndOtpRequired
    Nothing -> pure (DTC.isEndOtpRequired tripCategory)
