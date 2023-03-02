{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Action.UI.FarePolicy.FareProduct
  ( ListFareProductsRes (..),
    UpdateFareProductReq (..),
    listFareProducts,
    updateFareProduct,
  )
where

import Domain.Types.FarePolicy.FareProduct
import qualified Domain.Types.Person as SP
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy.FareProduct as SFareProduct

newtype ListFareProductsRes = ListFareProductsRes
  { list :: [FareProductAPIEntity]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema, ToJSON, FromJSON)

data UpdateFareProductReq = UpdateFareProductReq
  { enabled :: Bool,
    fareProductType :: FareProductType
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

listFareProducts :: (CacheFlow m r, EsqDBFlow m r) => SP.Person -> m ListFareProductsRes
listFareProducts person = do
  fareProducts <- SFareProduct.findEnabledByMerchantId person.merchantId
  pure $ ListFareProductsRes $ makeFareProductAPIEntity <$> fareProducts

updateFareProduct :: forall m r. (HedisFlow m r, EsqDBFlow m r) => SP.Person -> UpdateFareProductReq -> m APISuccess
updateFareProduct person updReq = do
  let merchantId = person.merchantId
  Esq.runTransaction $
    if updReq.enabled
      then SFareProduct.insertIfNotExist @m merchantId updReq.fareProductType
      else SFareProduct.delete merchantId updReq.fareProductType
  pure Success
