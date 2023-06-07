{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FarePolicy where

import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.FareProduct as FareProductD
import Domain.Types.Merchant
import Domain.Types.Vehicle.Variant (Variant (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FareProduct as FareProduct
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy as QFP
import Tools.Error
import Tools.Maps

fareProductFlowtoFarePolicyFlow :: FareProductD.FlowType -> FarePolicyD.FlowType
fareProductFlowtoFarePolicyFlow = \case
  FareProductD.NORMAL -> FarePolicyD.NORMAL
  FareProductD.RIDE_OTP -> FarePolicyD.RIDE_OTP

getFarePolicyForVariant :: (CacheFlow m r, EsqDBFlow m r, MonadReader r m, EsqDBReplicaFlow m r) => Id Merchant -> LatLong -> LatLong -> Variant -> m FarePolicyD.FullFarePolicy
getFarePolicyForVariant merchantId fromlocaton toLocation vehVariant = do
  fareProduct <- FareProduct.getFareProductForVariant merchantId fromlocaton toLocation vehVariant
  farePolicy <- QFP.findById fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
  return $ FarePolicyD.farePolicyToFullFarePolicy merchantId vehVariant (fareProductFlowtoFarePolicyFlow fareProduct.flow) farePolicy

getFarePolicyForVariants :: (CacheFlow m r, EsqDBFlow m r, MonadReader r m, EsqDBReplicaFlow m r) => Id Merchant -> LatLong -> LatLong -> m [FarePolicyD.FullFarePolicy]
getFarePolicyForVariants merchantId fromlocaton toLocation = do
  fareProducts <- FareProduct.getFareProductForVariants merchantId fromlocaton toLocation
  mapM
    ( \fareProduct -> do
        farePolicy <- QFP.findById fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
        return $ FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleVariant (fareProductFlowtoFarePolicyFlow fareProduct.flow) farePolicy
    )
    fareProducts
