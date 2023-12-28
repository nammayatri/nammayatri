{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.MerchantConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.MerchantConfig as DMC
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantConfig as BeamMC

findAllByMerchantOperatingCityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m [DMC.MerchantConfig]
findAllByMerchantOperatingCityId (Id merchantOperatingCityId) = findAllWithKV [Se.And [Se.Is BeamMC.merchantOperatingCityId $ Se.Eq merchantOperatingCityId, Se.Is BeamMC.enabled $ Se.Eq True]]

instance FromTType' BeamMC.MerchantConfig MerchantConfig where
  fromTType' BeamMC.MerchantConfigT {..} = do
    pure $
      Just
        MerchantConfig
          { id = Id id,
            merchantId = Id merchantId,
            merchantOperatingCityId = Id merchantOperatingCityId,
            fraudBookingCancellationCountThreshold = fraudBookingCancellationCountThreshold,
            fraudBookingCancellationCountWindow = fraudBookingCancellationCountWindow,
            fraudBookingTotalCountThreshold = fraudBookingTotalCountThreshold,
            fraudBookingCancelledByDriverCountThreshold = fraudBookingCancelledByDriverCountThreshold,
            fraudBookingCancelledByDriverCountWindow = fraudBookingCancelledByDriverCountWindow,
            fraudSearchCountThreshold = fraudSearchCountThreshold,
            fraudSearchCountWindow = fraudSearchCountWindow,
            fraudRideCountThreshold = fraudRideCountThreshold,
            fraudRideCountWindow = fraudRideCountWindow,
            enabled = enabled
          }

instance ToTType' BeamMC.MerchantConfig MerchantConfig where
  toTType' MerchantConfig {..} = do
    BeamMC.MerchantConfigT
      { BeamMC.id = getId id,
        BeamMC.merchantId = getId merchantId,
        BeamMC.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamMC.fraudBookingCancellationCountThreshold = fraudBookingCancellationCountThreshold,
        BeamMC.fraudBookingCancellationCountWindow = fraudBookingCancellationCountWindow,
        BeamMC.fraudBookingTotalCountThreshold = fraudBookingTotalCountThreshold,
        BeamMC.fraudBookingCancelledByDriverCountThreshold = fraudBookingCancelledByDriverCountThreshold,
        BeamMC.fraudBookingCancelledByDriverCountWindow = fraudBookingCancelledByDriverCountWindow,
        BeamMC.fraudSearchCountThreshold = fraudSearchCountThreshold,
        BeamMC.fraudSearchCountWindow = fraudSearchCountWindow,
        BeamMC.fraudRideCountThreshold = fraudRideCountThreshold,
        BeamMC.fraudRideCountWindow = fraudRideCountWindow,
        BeamMC.enabled = enabled
      }
