{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.MerchantConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant (Merchant)
import Domain.Types.MerchantConfig as DMC
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (findById)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Storage.Beam.MerchantConfig as BeamMC
import Storage.Tabular.MerchantConfig

findAllByMerchantId :: Transactionable m => Id Merchant -> m [DMC.MerchantConfig]
findAllByMerchantId merchantId =
  Esq.findAll $ do
    config <- from $ table @MerchantConfigT
    where_ $
      config ^. MerchantConfigMerchantId ==. val (toKey merchantId)
        &&. config ^. MerchantConfigEnabled ==. val True
    return config

transformBeamMerchantConfigToDomain :: BeamMC.MerchantConfig -> MerchantConfig
transformBeamMerchantConfigToDomain BeamMC.MerchantConfigT {..} = do
  MerchantConfig
    { id = Id id,
      merchantId = Id merchantId,
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

transformDomainMerchantConfigToBeam :: MerchantConfig -> BeamMC.MerchantConfig
transformDomainMerchantConfigToBeam MerchantConfig {..} =
  BeamMC.defaultMerchantConfig
    { BeamMC.id = getId id,
      BeamMC.merchantId = getId merchantId,
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
