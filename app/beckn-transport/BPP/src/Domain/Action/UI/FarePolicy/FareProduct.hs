{-# LANGUAGE DerivingVia #-}

module Domain.Action.UI.FarePolicy.FareProduct
  ( ListFareProductsRes (..),
    UpdateFareProductReq (..),
    listFareProducts,
    updateFareProduct,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis
import Beckn.Types.APISuccess
import Beckn.Utils.Common
import Domain.Types.FarePolicy.FareProduct
import qualified Domain.Types.Person as SP
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

updateFareProduct :: (HedisFlow m r, EsqDBFlow m r) => SP.Person -> UpdateFareProductReq -> m APISuccess
updateFareProduct person updReq = do
  let merchantId = person.merchantId
  Esq.runTransaction $
    if updReq.enabled
      then SFareProduct.insertIfNotExist merchantId updReq.fareProductType
      else SFareProduct.delete merchantId updReq.fareProductType
  SFareProduct.clearCache merchantId updReq.fareProductType
  pure Success
