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
import Tools.Error

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

listFareProducts :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => SP.Person -> m ListFareProductsRes
listFareProducts person = do
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organizationId")
  fareProducts <- SFareProduct.findEnabledByOrgId orgId
  pure $ ListFareProductsRes $ makeFareProductAPIEntity <$> fareProducts

updateFareProduct :: (HedisFlow m r, EsqDBFlow m r) => SP.Person -> UpdateFareProductReq -> m APISuccess
updateFareProduct person updReq = do
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organizationId")
  Esq.runTransaction $
    if updReq.enabled
      then SFareProduct.insertIfNotExist orgId updReq.fareProductType
      else SFareProduct.delete orgId updReq.fareProductType
  SFareProduct.clearCache orgId updReq.fareProductType
  pure Success
