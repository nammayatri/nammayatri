{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FraudConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.FraudConfig as DFC
import Domain.Types.Merchant (Merchant)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Sequelize as Se
import qualified Storage.Beam.FraudConfig as BeamFC

findAllByMerchantId :: MonadFlow m => Id Merchant -> m [DFC.FraudConfig]
findAllByMerchantId (Id merchantId) = findAllWithKV [Se.And [Se.Is BeamFC.merchantId $ Se.Eq merchantId, Se.Is BeamFC.enabled $ Se.Eq True]]

instance FromTType' BeamFC.FraudConfig FraudConfig where
  fromTType' BeamFC.FraudConfigT {..} = do
    pure $
      Just
        FraudConfig
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

instance ToTType' BeamFC.FraudConfig FraudConfig where
  toTType' FraudConfig {..} = do
    BeamFC.FraudConfigT
      { BeamFC.id = getId id,
        BeamFC.merchantId = getId merchantId,
        BeamFC.fraudBookingCancellationCountThreshold = fraudBookingCancellationCountThreshold,
        BeamFC.fraudBookingCancellationCountWindow = fraudBookingCancellationCountWindow,
        BeamFC.fraudBookingTotalCountThreshold = fraudBookingTotalCountThreshold,
        BeamFC.fraudBookingCancelledByDriverCountThreshold = fraudBookingCancelledByDriverCountThreshold,
        BeamFC.fraudBookingCancelledByDriverCountWindow = fraudBookingCancelledByDriverCountWindow,
        BeamFC.fraudSearchCountThreshold = fraudSearchCountThreshold,
        BeamFC.fraudSearchCountWindow = fraudSearchCountWindow,
        BeamFC.fraudRideCountThreshold = fraudRideCountThreshold,
        BeamFC.fraudRideCountWindow = fraudRideCountWindow,
        BeamFC.enabled = enabled
      }
